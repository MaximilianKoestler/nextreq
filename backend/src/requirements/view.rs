use super::model::Requirement;

pub trait RequirementsView: Send + Sync + 'static {
    fn by_id(&self, id: &str) -> &Requirement;
}
