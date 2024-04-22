pub type HvmlLab = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CtrKind {
  Con(Option<HvmlLab>),
  Tup(Option<HvmlLab>),
  Dup(HvmlLab),
}

impl CtrKind {
  pub fn to_lab(self) -> HvmlLab {
    #[allow(clippy::identity_op)]
    match self {
      CtrKind::Con(None) => 0,
      CtrKind::Con(Some(x)) => ((x + 1) << 2) | 0b00,
      CtrKind::Tup(None) => 1,
      CtrKind::Tup(Some(x)) => ((x + 1) << 2) | 0b01,
      CtrKind::Dup(x) => (x << 2) | 0b10,
    }
  }
  pub fn from_lab(lab: u16) -> Self {
    match (lab >> 2, lab & 0b11) {
      (0, 0b00) => CtrKind::Con(None),
      (x, 0b00) => CtrKind::Con(Some(x - 1)),
      (0, 0b01) => CtrKind::Tup(None),
      (x, 0b01) => CtrKind::Tup(Some(x - 1)),
      (x, 0b10) => CtrKind::Dup(x),
      _ => unreachable!(),
    }
  }
}
