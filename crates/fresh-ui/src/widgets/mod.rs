//! The widget set.
//!
//! Every widget here is an ordinary `Component` over the seven primitives, with
//! no privileged access to the framework — application code can write any of
//! them, and can replace any of them. The cost of that rule is verbosity in the
//! description trees; the mitigation is convenience constructors, not a
//! privileged primitive.
//!
//! ```text
//! Button   = Focusable(Gesture(Box(TextRun)))
//! Toggle   = Focusable(Gesture(Box([TextRun(mark), TextRun(label)])))
//! List     = Viewport(Box::col(rows))            + selection state
//! Dropdown = Button, and a Layer when open
//! ```
//!
//! Widgets follow the controlled/uncontrolled split: pass a value and a change
//! handler and the owner holds the state; omit them and the element does.

pub mod button;
pub mod field;
pub mod list;
pub mod menu;
pub mod misc;

pub use button::{Button, Toggle};
pub use field::{Number, TextField};
pub use list::{List, Tree, TreeNode};
pub use menu::{Dropdown, RadioGroup};
pub use misc::{divider, spacer};
