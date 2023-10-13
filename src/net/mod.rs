#![allow(non_snake_case)]

pub mod hvmc_to_net;
pub mod inter_net;
pub mod net_to_hvmc;

pub use hvmc_to_net::core_net_to_compat;
pub use net_to_hvmc::{compat_net_to_core, nets_to_hvm_core};
