use std::collections::HashMap;

use super::control::RequirementsControl;
use super::model::Requirement;
use super::view::RequirementsView;

pub struct RequirementsInteractor {
    next_id: usize,
    requirements: HashMap<String, Requirement>,
}

impl RequirementsInteractor {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            requirements: HashMap::new(),
        }
    }
}

impl RequirementsView for RequirementsInteractor {
    fn by_id(&self, id: &str) -> &Requirement {
        self.requirements.get(id).unwrap()
    }
}

impl RequirementsControl for RequirementsInteractor {
    fn new(&mut self) -> String {
        let id = self.next_id.to_string();
        self.next_id += 1;

        self.requirements.insert(
            id.clone(),
            Requirement {
                text: "".to_owned(),
            },
        );
        id
    }

    fn update_by_id(&mut self, id: &str, text: &str) {
        self.requirements.get_mut(id).unwrap().text = text.to_owned();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn text(input: String) {
            let mut requirements = RequirementsInteractor::new();

            let id = {
                let control = &mut requirements as &mut dyn RequirementsControl;
                let id = control.new();
                control.update_by_id(&id, &input);
                id
            };

            {
                let view = &requirements as &dyn RequirementsView;
                let requirement = view.by_id(&id);
                assert_eq!(requirement.text, input);
            }
        }
    }
}
