use std::collections::HashMap;
use std::path::Path;

use proc_macro2::TokenStream;
use serde::Deserialize;

use crate::BindgenContext;
use crate::clang::ABIKind;
use crate::ir::layout::Layout;
use crate::ir::context::TypeId;

#[derive(Debug, Clone, Deserialize)]
pub struct EncapfnFunctionConfig {
    pub fntab_id: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub struct EncapfnConfig {
    pub wrapper_name: String,
    pub functions: HashMap<String, EncapfnFunctionConfig>,
}

#[derive(Debug)]
pub struct EncapfnContext {
    pub config: EncapfnConfig,
}

impl EncapfnContext {
    pub fn new<P: AsRef<Path>>(cfg: &P) -> Self {
	EncapfnContext {
            config: toml::from_str(std::str::from_utf8(&std::fs::read(cfg).unwrap()).unwrap()).unwrap(),
	}
    }

    pub fn prologue(&self) -> TokenStream {
	let wrapper_name_ident = format_ident!("{}", &self.config.wrapper_name);

	quote!{
	    pub enum #wrapper_name_ident<ID: ::encapfn::branding::EFID, RT: ::encapfn::EncapfnRt> {
		_Impossible(::core::convert::Infallible, ::core::marker::PhantomData<ID>, ::core::marker::PhantomData<RT>),
	    }
	}
    }

    pub fn get_oracle_for_triple<'a>(&self, ctx: &'a BindgenContext, triple: &str, abi: ABIKind) -> Option<Box<dyn EncapfnABIOracle + 'a>> {
	match (triple, abi) {
	    ("riscv32-unknown-unknown", ABIKind::GenericItanium) => {
		Some(Box::new(EncapfnRv32iCOracle::new(ctx)))
	    },

	    (_, _) => None,
	}
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ArgumentSlot {
    ArgumentRegister(usize, usize),
    Stacked(usize, usize),
}

pub trait EncapfnABIOracle {
    fn determine_argument_slots(&self, args: &Vec<Layout>) -> Vec<ArgumentSlot>;
    fn determine_stack_spill(&self, args: &Vec<Layout>) -> usize;
    fn argument_slot_type(&self, arg_slot: ArgumentSlot) -> TokenStream;
}

pub struct EncapfnRv32iCOracle<'a> {
    ctx: &'a BindgenContext,
}

impl<'a> EncapfnRv32iCOracle<'a> {
    pub fn new(ctx: &'a BindgenContext) -> Self {
	EncapfnRv32iCOracle {
	    ctx,
	}
    }
}

fn rv_determine_argument_slots<const RV64: bool>(args: &Vec<Layout>) -> Vec<ArgumentSlot> {
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
	let double_pointer_word = arg.size > ptr_size && arg.size <= 2 * ptr_size;

	// If we have a type twice the pointer size, make sure to
	// place it at an even offset:
	if double_pointer_word && ptr_offset % 2 == 1 {
	    ptr_offset += 1;
	}

	// No matter what, place the argument at this index. On RISC-V
	// we have 8 argument registers available. Either place it in
	// one of those, or spill on the stack:
	let width = if arg.size > 2 * ptr_size { ptr_size } else { arg.size };
	if let Some(stack_offset) = ptr_offset.checked_sub(8) {
	    slots.push(ArgumentSlot::Stacked(
		stack_offset,
		width,
	    ));
	} else {
	    slots.push(ArgumentSlot::ArgumentRegister(
		ptr_offset,
		width,
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
    fn determine_argument_slots(&self, args: &Vec<Layout>) -> Vec<ArgumentSlot> {
	rv_determine_argument_slots::<false>(args)
    }

    fn determine_stack_spill(&self, args: &Vec<Layout>) -> usize {
	match self.determine_argument_slots(args).last() {
	    Some(ArgumentSlot::Stacked(offset, width)) => {
		// We pass by reference for arguments larger than two pointers
		assert!(*width <= 8);
		if *width <= 4 {
		    *offset + 1
		} else {
		    *offset + 2
		}
	    },
	    Some(ArgumentSlot::ArgumentRegister(_, _)) => 0,
	    None => 0,
	}
    }

    fn argument_slot_type(&self, arg_slot: ArgumentSlot) -> TokenStream {
	match arg_slot {
	    ArgumentSlot::ArgumentRegister(idx, _) => {
		let reg_type = format_ident!("AREG{}", idx);
		quote! {
		    ::encapfn::abi::calling_convention::#reg_type<::encapfn::abi::rv32i_c::Rv32iCABI>
		}
	    },
	    ArgumentSlot::Stacked(offset, _) => {
		quote! {
		    ::encapfn::abi::calling_convention::Stacked<#offset, ::encapfn::abi::rv32i_c::Rv32iCABI>
		}
	    },
	}
    }
}
