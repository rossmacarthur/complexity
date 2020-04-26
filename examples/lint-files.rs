//! This example calculates the cognitive complexity for each function and
//! method in a Rust file.

use std::{
    fs,
    path::{Path, PathBuf},
};

use ansi_term::Color::{Blue, Red};
use anyhow::{Context, Result};
use complexity::Complexity;
use structopt::StructOpt;
use syn::{self, File, ImplItem, Item, Type, TypePath};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Function {
    complexity: u32,
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
                                let name = format!(
                                    "{}::{}",
                                    path.segments.last().unwrap().ident,
                                    method.sig.ident
                                );
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

fn resolve_paths(paths: Vec<PathBuf>) -> Result<Vec<PathBuf>> {
    let mut result = Vec::with_capacity(paths.len());
    for path in paths {
        if path.is_dir() {
            result.extend(
                walkdir::WalkDir::new(path)
                    .into_iter()
                    .filter_map(Result::ok)
                    .filter(|e| e.path().extension().map(|s| s == "rs").unwrap_or(false))
                    .map(walkdir::DirEntry::into_path),
            );
        } else {
            result.push(path)
        }
    }
    Ok(result)
}

fn run(paths: Vec<PathBuf>, max_complexity: &Option<u32>) -> Result<bool> {
    let mut table = tabular::Table::new("{:<}    {:>}");
    let mut result = false;
    for path in resolve_paths(paths)? {
        let mut functions = lint(&path)?;
        functions.sort();
        for (index, function) in functions
            .into_iter()
            .rev()
            .filter(|f| max_complexity.map(|m| f.complexity >= m).unwrap_or(true))
            .enumerate()
        {
            if index == 0 {
                table.add_heading(format!(
                    "\n{}: {}",
                    Blue.bold().paint("file"),
                    Blue.bold().paint(path.display().to_string()),
                ));
            }
            table.add_row(tabular::row!(function.name, function.complexity));
            result = true;
        }
    }
    print!("{}", table);
    Ok(result)
}

#[derive(Debug, StructOpt)]
struct Opt {
    /// One or more files to calculate cognitive complexity for.
    #[structopt(name = "PATH")]
    paths: Vec<PathBuf>,
    /// Require functions/methods that have a complexity greater than or equal
    /// to this.
    #[structopt(short, long, name = "INT")]
    max_complexity: Option<u32>,
}

fn main() -> Result<()> {
    let Opt {
        paths,
        max_complexity,
    } = Opt::from_args();

    if !run(paths, &max_complexity)? {
        if let Some(max_complexity) = max_complexity {
            eprintln!(
                "\n{}",
                Red.paint(format!(
                    "The indicated methods and functions did not meet the required complexity of \
                     {}",
                    max_complexity
                )),
            );
            std::process::exit(1);
        }
    }
    Ok(())
}
