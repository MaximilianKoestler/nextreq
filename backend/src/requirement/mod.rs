mod control;
mod view;

use control::RequirementControl;
use view::RequirementView;

pub struct Requirement {
    text: String,
}

impl Requirement {
    pub fn new() -> Self {
        Self {
            text: "".to_owned(),
        }
    }
}

impl RequirementView for Requirement {
    fn text(&self) -> &str {
        &self.text
    }
}

impl RequirementControl for Requirement {
    fn set_text(&mut self, text: &str) {
        self.text = text.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn text(input: String) {
            let mut requirement = Requirement::new();
            {
                let view = &requirement as &dyn RequirementView;
                assert_eq!(view.text(), "");
            }

            {
                let control = &mut requirement as &mut dyn RequirementControl;
                control.set_text(&input);
            }

            {
                let view = &requirement as &dyn RequirementView;
                assert_eq!(view.text(), input);
            }
        }
    }
}
