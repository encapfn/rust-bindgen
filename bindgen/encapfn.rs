use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;

use proc_macro2::{Ident, Literal, TokenStream};
use serde::Deserialize;

use crate::clang::ABIKind;
use crate::ir::layout::Layout;
use crate::BindgenContext;

#[derive(Debug, Clone, Deserialize)]
pub struct EncapfnFunctionConfig {
    pub fntab_id: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub struct EncapfnConfig {
    pub wrapper_name: String,
    pub functions: HashMap<String, EncapfnFunctionConfig>,
}

// #[derive(Debug)]
pub struct EncapfnContext {
    pub config: EncapfnConfig,
    pub symbol_table_offsets: (HashMap<String, usize>, Vec<String>),
    pub trait_functions: RefCell<Vec<TokenStream>>,
    pub abi_trait_implementations: RefCell<
        HashMap<
            String,
            (
                Box<
                    dyn Fn(
                        &Ident,
                        &Ident,
                        &[TokenStream],
                        &[TokenStream],
                    ) -> TokenStream,
                >,
                Vec<TokenStream>,
                Vec<TokenStream>,
            ),
        >,
    >,
}

impl std::fmt::Debug for EncapfnContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EncapfnContext")
            .field("config", &self.config)
            .field("trait_functions", &self.config)
            .field("abi_trait_implementations", &"<elided>")
            .finish()
    }
}

impl EncapfnContext {
    pub fn new<P: AsRef<Path>>(cfg: &P) -> Self {
        let config: EncapfnConfig = toml::from_str(
            std::str::from_utf8(&std::fs::read(cfg).unwrap()).unwrap(),
        )
        .unwrap();

        let symbol_table = config
            .functions
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>();
        let symbol_table_offsets = symbol_table
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.clone(), idx))
            .collect::<HashMap<_, _>>();

        EncapfnContext {
            config,
            symbol_table_offsets: (symbol_table_offsets, symbol_table),
            trait_functions: RefCell::new(vec![]),
            abi_trait_implementations: RefCell::new(HashMap::new()),
        }
    }

    pub fn prologue(&self) -> TokenStream {
        fn symbol_to_cstr_lit(sym: &str) -> TokenStream {
            // quote doesn't have first-class CStr support, so need to
            // construct ourselves using a `Verbatim` literal. We are
            // conservative in the symbol names we accept for now, to avoid
            // generating invalid syntax:
            assert!(sym.chars().all(|c| {
                c.is_ascii_alphanumeric() || c == '_' || c == '-'
            }));
            let lit = <Literal as FromStr>::from_str(&format!("c\"{}\"", sym))
                .unwrap();
            quote! { #lit }
        }

        let wrapper_trait_ident =
            format_ident!("{}", &self.config.wrapper_name);
        let wrapper_type_ident =
            format_ident!("{}Rt", &self.config.wrapper_name);
        let fixed_offset_function_table_ident =
            format_ident!("{}FixedFntab", &self.config.wrapper_name);
        let function_table_ident =
            format_ident!("{}Fntab", &self.config.wrapper_name);

        let function_definitions = self.trait_functions.borrow();

        let abi_trait_implementations = self
            .abi_trait_implementations
            .borrow()
            .iter()
            .map(|(_abi_label, (template_fn, rt_constraints, impls))| {
                template_fn(
                    &wrapper_trait_ident,
                    &wrapper_type_ident,
                    &rt_constraints,
                    &impls,
                )
            })
            .collect::<Vec<_>>();

        let mut function_table_entries = self
            .config
            .functions
            .iter()
            .map(|(name, cfg)| (name, cfg.fntab_id))
            .collect::<Vec<_>>();
        function_table_entries.sort_by_key(|(_, fntab_id)| *fntab_id);

        let fixed_function_table_length: usize = function_table_entries
            .last()
            .map(|(_, fntab_id)| (*fntab_id as usize) + 1)
            .unwrap_or(0);

        let mut fixed_function_table_sequence =
            Vec::with_capacity(fixed_function_table_length);
        for (symbol, fntab_id) in function_table_entries {
            while fixed_function_table_sequence.len() < fntab_id as usize {
                fixed_function_table_sequence.push(quote! { None });
            }

            let cstr_lit = symbol_to_cstr_lit(symbol);
            fixed_function_table_sequence.push(quote! { Some(#cstr_lit) });
        }

        let (_, function_table_sequence_str) = &self.symbol_table_offsets;
        let function_table_sequence = function_table_sequence_str
            .iter()
            .map(|symbol| symbol_to_cstr_lit(symbol))
            .collect::<Vec<_>>();
        let function_table_length = function_table_sequence.len();

        quote! {
            // This must be generic over both the RT _and_ ABI, despite the ABI being
            // governed by the RT. Otherwise, we cannot provide different implementations
            // of this for different ABIs:
            // https://geo-ant.github.io/blog/2021/mutually-exclusive-traits-rust/
            pub trait #wrapper_trait_ident<
                ID: ::encapfn::branding::EFID,
                RT: ::encapfn::rt::EncapfnRt<ID = ID>,
                ABI = <RT as ::encapfn::rt::EncapfnRt>::ABI
            > {
                type RT: ::encapfn::rt::EncapfnRt;

                fn rt(&self) -> &Self::RT;

                #( #function_definitions )*
            }

            pub const #function_table_ident: [
                &'static ::core::ffi::CStr; #function_table_length
            ] = [ #( #function_table_sequence ),* ];

            pub const #fixed_offset_function_table_ident: [
                Option<&'static ::core::ffi::CStr>; #fixed_function_table_length
            ] = [ #( #fixed_function_table_sequence ),* ];

            pub struct #wrapper_type_ident<
                'a,
                ID: ::encapfn::branding::EFID,
                RT: ::encapfn::rt::EncapfnRt,
            > {
                rt: &'a RT,
                symbols: RT::SymbolTableState<#function_table_length, #fixed_function_table_length>,
                _id: ::core::marker::PhantomData<ID>,
            }

            impl<
                'a,
                ID: ::encapfn::branding::EFID,
                RT: ::encapfn::rt::EncapfnRt,
            > #wrapper_type_ident<'a, ID, RT> {
                pub fn new(rt: &'a RT) -> Option<Self> {
                    if let Some(symbols) = rt.resolve_symbols(
                        &#function_table_ident,
                        &#fixed_offset_function_table_ident
                    ) {
                        Some(#wrapper_type_ident {
                            rt: rt,
                            symbols,
                            _id: ::core::marker::PhantomData,
                        })
                    } else {
                        None
                    }
                }
            }

            #( #abi_trait_implementations )*
        }
    }

    pub fn get_oracle_for_triple<'a>(
        &self,
        ctx: &'a BindgenContext,
        triple: &str,
        abi: ABIKind,
    ) -> Option<Box<dyn EncapfnABIOracle + 'a>> {
        match (triple, abi) {
            ("riscv32-unknown-unknown", ABIKind::GenericItanium) |
            ("riscv32-unknown-none-elf", ABIKind::GenericItanium) => {
                Some(Box::new(EncapfnRv32iCOracle::new(ctx)))
            }

            ("x86_64-unknown-linux-gnu", ABIKind::GenericItanium) => {
                Some(Box::new(EncapfnSysVAMD64Oracle::new(ctx)))
            }

            (_, _) => None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ArgumentSlot {
    ArgumentRegister(usize, usize, bool),
    Stacked(usize, usize, bool),
}

impl ArgumentSlot {
    #[allow(unused)]
    pub fn get_width(&self) -> usize {
        match self {
            ArgumentSlot::ArgumentRegister(_, w, _) => *w,
            ArgumentSlot::Stacked(_, w, _) => *w,
        }
    }

    pub fn pass_by_ref(&self) -> bool {
        match self {
            ArgumentSlot::ArgumentRegister(_, _, pbr) => *pbr,
            ArgumentSlot::Stacked(_, _, pbr) => *pbr,
        }
    }
}

pub trait EncapfnABIOracle {
    fn determine_argument_slots(&self, args: &Vec<Layout>)
        -> Vec<ArgumentSlot>;
    fn determine_stack_spill(&self, args: &Vec<Layout>) -> usize;
    fn argument_slot_type(&self, arg_slot: ArgumentSlot) -> TokenStream;
    // fn abi_label(&self) -> &'static str;
    // fn abi_type(&self) -> TokenStream;
}

pub struct EncapfnRv32iCOracle<'a> {
    _ctx: &'a BindgenContext,
}

impl<'a> EncapfnRv32iCOracle<'a> {
    pub fn new(ctx: &'a BindgenContext) -> Self {
        EncapfnRv32iCOracle { _ctx: ctx }
    }
}

fn rv_determine_argument_slots<const RV64: bool>(
    args: &Vec<Layout>,
) -> Vec<ArgumentSlot> {
    // RISC-V is rather simple in how it passes arguments. It
    // fills up argument registers from a0 to a7, and then
    // proceedes to spill on the stack, where the first spilled
    // argument is located at a 0-byte offset from the stack
    // pointer.
    //
    // Arguments smaller than a pointer word are always contained
    // in the lower addresses of a pointer word, in both registers
    // and on the stack.
    //
    // Arguments twice the size of a pointer word are passed in
    // registers or on the stack, but will be aligned to an even
    // offset of registers or pointer-words on the stack.
    //
    // Larger arguments will be passed by reference, thus we treat
    // them as pointer-sized.

    let ptr_size = if RV64 { 8 } else { 4 };

    // Keep track of the current offset in pointer-words:
    let mut ptr_offset: usize = 0;

    // Produce argument slots along the way:
    let mut slots = Vec::with_capacity(args.len());

    // Iterate over the type-layout of all arguments:
    for arg in args {
        let double_pointer_word =
            arg.size > ptr_size && arg.size <= 2 * ptr_size;

        // If we have a type twice the pointer size, make sure to
        // place it at an even offset:
        if double_pointer_word && ptr_offset % 2 == 1 {
            ptr_offset += 1;
        }

        // No matter what, place the argument at this index. On RISC-V
        // we have 8 argument registers available. Either place it in
        // one of those, or spill on the stack:
        let (width, pass_by_ref) = if arg.size > 2 * ptr_size {
            (ptr_size, true)
        } else {
            (arg.size, false)
        };
        if let Some(stack_offset) = ptr_offset.checked_sub(8) {
            slots.push(ArgumentSlot::Stacked(stack_offset, width, pass_by_ref));
        } else {
            slots.push(ArgumentSlot::ArgumentRegister(
                ptr_offset,
                width,
                pass_by_ref,
            ));
        }

        // Now, if the argument as twice the pointer size, increment
        // our offset by two pointer-size words, otherwise one:
        if double_pointer_word {
            ptr_offset += 2;
        } else {
            ptr_offset += 1;
        }
    }

    slots
}

impl<'a> EncapfnABIOracle for EncapfnRv32iCOracle<'a> {
    fn determine_argument_slots(
        &self,
        args: &Vec<Layout>,
    ) -> Vec<ArgumentSlot> {
        rv_determine_argument_slots::<false>(args)
    }

    fn determine_stack_spill(&self, args: &Vec<Layout>) -> usize {
        match self.determine_argument_slots(args).last() {
            Some(ArgumentSlot::Stacked(offset, width, _)) => {
                // We pass by reference for arguments larger than two pointers
                assert!(*width <= 8);
                if *width <= 4 {
                    *offset + 1
                } else {
                    *offset + 2
                }
            }
            Some(ArgumentSlot::ArgumentRegister(_, _, _)) => 0,
            None => 0,
        }
    }

    fn argument_slot_type(&self, arg_slot: ArgumentSlot) -> TokenStream {
        match arg_slot {
            ArgumentSlot::ArgumentRegister(idx, _, _) => {
                let reg_type = format_ident!("AREG{}", idx);
                quote! {
                    ::encapfn::abi::calling_convention::#reg_type<::encapfn::abi::rv32i_c::Rv32iCABI>
                }
            }
            ArgumentSlot::Stacked(offset, _, _) => {
                let offset_bytes = offset * 8;
                quote! {
                    ::encapfn::abi::calling_convention::Stacked<#offset_bytes, ::encapfn::abi::rv32i_c::Rv32iCABI>
                }
            }
        }
    }
}

// TODO: currently just copied from rv32i, actually implemented for SysV-AMD64!
pub struct EncapfnSysVAMD64Oracle<'a> {
    _ctx: &'a BindgenContext,
}

impl<'a> EncapfnSysVAMD64Oracle<'a> {
    pub fn new(ctx: &'a BindgenContext) -> Self {
        EncapfnSysVAMD64Oracle { _ctx: ctx }
    }
}

impl<'a> EncapfnABIOracle for EncapfnSysVAMD64Oracle<'a> {
    fn determine_argument_slots(
        &self,
        args: &Vec<Layout>,
    ) -> Vec<ArgumentSlot> {
        // TODO: this is still mostly just the RISC-V code!
        const PTR_SIZE: usize = 8;

        // Keep track of the current offset in pointer-words:
        let mut ptr_offset: usize = 0;

        // Produce argument slots along the way:
        let mut slots = Vec::with_capacity(args.len());

        // Iterate over the type-layout of all arguments:
        for arg in args {
            let double_pointer_word =
                arg.size > PTR_SIZE && arg.size <= 2 * PTR_SIZE;

            // If we have a type twice the pointer size, make sure to
            // place it at an even offset:
            if double_pointer_word && ptr_offset % 2 == 1 {
                ptr_offset += 1;
            }

            // No matter what, place the argument at this index. In the System-V
            // AMD64 ABI we have 6 argument registers available. Either place it
            // in one of those, or spill on the stack:
            let (width, pass_by_ref) = if arg.size > 2 * PTR_SIZE {
                (PTR_SIZE, true)
            } else {
                (arg.size, false)
            };
            if let Some(stack_offset) = ptr_offset.checked_sub(6) {
                slots.push(ArgumentSlot::Stacked(
                    stack_offset,
                    width,
                    pass_by_ref,
                ));
            } else {
                slots.push(ArgumentSlot::ArgumentRegister(
                    ptr_offset,
                    width,
                    pass_by_ref,
                ));
            }

            // Now, if the argument as twice the pointer size, increment
            // our offset by two pointer-size words, otherwise one:
            if double_pointer_word {
                ptr_offset += 2;
            } else {
                ptr_offset += 1;
            }
        }

        slots
    }

    fn determine_stack_spill(&self, args: &Vec<Layout>) -> usize {
        match self.determine_argument_slots(args).last() {
            Some(ArgumentSlot::Stacked(offset, width, _)) => {
                // We pass by reference for arguments larger than two pointers
                assert!(*width <= 8);
                if *width <= 4 {
                    *offset + 1
                } else {
                    *offset + 2
                }
            }
            Some(ArgumentSlot::ArgumentRegister(_, _, _)) => 0,
            None => 0,
        }
    }

    fn argument_slot_type(&self, arg_slot: ArgumentSlot) -> TokenStream {
        match arg_slot {
            ArgumentSlot::ArgumentRegister(idx, _, _) => {
                let reg_type = format_ident!("AREG{}", idx);
                quote! {
                    ::encapfn::abi::calling_convention::#reg_type<::encapfn::abi::sysv_amd64::SysVAMD64ABI>
                }
            }
            ArgumentSlot::Stacked(offset, _, _) => {
                let offset_bytes = offset * 8;
                quote! {
                    ::encapfn::abi::calling_convention::Stacked<#offset_bytes, ::encapfn::abi::sysv_amd64::SysVAMD64ABI>
                }
            }
        }
    }
}
