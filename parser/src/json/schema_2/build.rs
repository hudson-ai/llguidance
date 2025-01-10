// use anyhow::Result;
// use referencing::{Draft, Registry, Resource, Retrieve};
// use serde_json::Value;
// use std::{any::type_name_of_val, rc::Rc};

// use super::preschema::PreSchema;

// const DEFAULT_ROOT_URI: &str = "json-schema:///";
// const DEFAULT_DRAFT: Draft = Draft::Draft202012;

// fn draft_for(value: &Value) -> Draft {
//     DEFAULT_DRAFT.detect(value).unwrap_or(DEFAULT_DRAFT)
// }

// #[derive(Clone)]
// pub struct RetrieveWrapper(pub Rc<dyn Retrieve>);
// impl RetrieveWrapper {
//     pub fn new(retrieve: Rc<dyn Retrieve>) -> Self {
//         RetrieveWrapper(retrieve)
//     }
// }
// impl std::ops::Deref for RetrieveWrapper {
//     type Target = dyn Retrieve;
//     fn deref(&self) -> &Self::Target {
//         self.0.as_ref()
//     }
// }
// impl std::fmt::Debug for RetrieveWrapper {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", type_name_of_val(&self.0))
//     }
// }

// pub fn preprocess_schema(contents: Value, retriever: Option<&dyn Retrieve>) -> Result<PreSchema> {
//     let draft = draft_for(&contents);
//     let resource = draft.create_resource(contents);
//     let base_uri = resource.id().unwrap_or(DEFAULT_ROOT_URI).to_string();

//     let registry = {
//         // Weirdly no apparent way to instantiate a new registry with a retriever, so we need to
//         // make an empty one and then add the retriever + resource that may depend on said retriever
//         let empty_registry =
//             Registry::try_from_resources(std::iter::empty::<(String, Resource)>())?;
//         empty_registry.try_with_resource_and_retriever(
//             &base_uri,
//             resource,
//             retriever.unwrap_or(&referencing::DefaultRetriever),
//         )?
//     };

//     let resolver = registry.try_resolver(&base_uri)?;
//     let ctx = Context {
//         resolver: resolver,
//         draft: draft,
//         shared: Rc::new(RefCell::new(SharedContext::new())),
//         options: SchemaBuilderOptions::default(),
//     };

//     let root_resource = ctx.lookup_resource(&base_uri)?;
//     let schema = compile_resource(&ctx, root_resource)?;
//     let defs = std::mem::take(&mut ctx.shared.borrow_mut().defs);
//     Ok((schema, defs))
// }
