pub trait RequirementsControl: Send + Sync + 'static {
    fn new(&mut self) -> String;
    fn update_by_id(&mut self, id: &str, text: &str);
}
