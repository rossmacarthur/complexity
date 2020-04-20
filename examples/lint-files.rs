//! This example calculates the cognitive complexity for each function and
//! method in a Rust file.

use std::{
    fs,
    path::{Path, PathBuf},
};

use ansi_term::Color::Blue;
use anyhow::{Context, Result};
use complexity::Complexity;
use structopt::StructOpt;
use syn::{self, File, ImplItem, Item, Type, TypePath};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Function {
    complexity: u64,
    name: String,
}

fn parse_file(path: &Path) -> Result<File> {
    let contents = fs::read_to_string(path)
        .with_context(|| format!("failed to read file `{}`", path.display()))?;
    let file = syn::parse_file(&contents)
        .with_context(|| format!("failed to parse syntax of `{}`", path.display()))?;
    Ok(file)
}

fn lint(path: &Path) -> Result<Vec<Function>> {
    let file = parse_file(path)?;
    let mut functions = Vec::new();
    for item in &file.items {
        match item {
            // An impl block like `impl Struct { .. }` or `impl Display for Struct { .. }`
            Item::Impl(item_impl) => {
                for impl_item in &item_impl.items {
                    if let ImplItem::Method(method) = impl_item {
                        match &*item_impl.self_ty {
                            Type::Path(TypePath { qself: None, path }) => {
                                let ty = path.segments.last().unwrap().ident.to_string();
                                let fn_name = method.sig.ident.to_string();
                                let name = format!("{}::{}", ty, fn_name);
                                let complexity = method.complexity();
                                functions.push(Function { name, complexity });
                            }
                            _ => {}
                        }
                    }
                }
            }
            // A bare function like `fn function(arg: Arg) -> Result { .. }`
            Item::Fn(item_fn) => {
                let name = item_fn.sig.ident.to_string();
                let complexity = item_fn.complexity();
                functions.push(Function { name, complexity });
            }
            _ => {}
        }
    }
    Ok(functions)
}

#[derive(Debug, StructOpt)]
struct Opt {
    /// One or more files to calculate cognitive complexity for.
    #[structopt(name = "PATH")]
    paths: Vec<PathBuf>,
}

fn main() -> Result<()> {
    let opt = Opt::from_args();
    let mut table = tabular::Table::new("{:<}    {:>}");
    for path in opt.paths {
        let mut functions = lint(&path)?;
        functions.sort();
        table.add_heading(format!(
            "\n{}: {}",
            Blue.bold().paint("file"),
            Blue.bold().paint(path.display().to_string()),
        ));
        for function in functions.into_iter().rev() {
            table.add_row(tabular::row!(function.name, function.complexity));
        }
    }
    print!("{}", table);
    Ok(())
}
