/// Demonstration of the file explorer functionality
///
/// This example shows how to use the file tree, view, and renderer components.
///
/// To run: cargo run --example file_explorer_demo

use fresh::file_tree::{FileTree, FileTreeView};
use fresh::fs::{FsManager, LocalFsBackend};
use fresh::ui::FileExplorerRenderer;
use std::env;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;

#[tokio::main]
async fn main() -> io::Result<()> {
    // Get the directory to explore (current directory by default)
    let path = env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| env::current_dir().unwrap());

    println!("File Explorer Demo");
    println!("==================");
    println!("Exploring: {}", path.display());
    println!();

    // Create filesystem backend and manager
    let backend = Arc::new(LocalFsBackend::new());
    let manager = Arc::new(FsManager::new(backend));

    // Create file tree
    let tree = FileTree::new(path.clone(), manager).await?;
    println!("✓ Created file tree rooted at: {}", path.display());
    println!("  Initial node count: {}", tree.node_count());
    println!();

    // Create view
    let mut view = FileTreeView::new(tree);
    println!("✓ Created file tree view");
    println!("  Visible nodes: {}", view.visible_count());
    println!();

    // Expand root directory
    println!("Expanding root directory...");
    let root_id = view.tree().root_id();
    view.tree_mut().expand_node(root_id).await?;
    println!("✓ Expanded root");
    println!("  Visible nodes: {}", view.visible_count());
    println!("  Node count: {}", view.tree().node_count());
    println!();

    // Display visible nodes
    println!("Visible entries:");
    println!("----------------");
    let display_nodes = view.get_display_nodes();
    for (node_id, indent) in display_nodes.iter().take(20) {
        if let Some(node) = view.tree().get_node(*node_id) {
            let indent_str = "  ".repeat(*indent);
            let icon = if node.is_dir() {
                if node.is_expanded() { "[-]" } else { "[+]" }
            } else {
                "[F]"
            };
            let name = &node.entry.name;
            println!("{}{} {}", indent_str, icon, name);

            if let Some(metadata) = &node.entry.metadata {
                if let Some(size) = metadata.size {
                    if !node.is_dir() {
                        println!("{}   Size: {} bytes", indent_str, size);
                    }
                }
            }
        }
    }

    if display_nodes.len() > 20 {
        println!("... and {} more", display_nodes.len() - 20);
    }

    println!();
    println!("Demo complete!");
    println!();
    println!("Integration notes:");
    println!("------------------");
    println!("• The file tree uses lazy loading - directories are only read when expanded");
    println!("• All filesystem operations are async and non-blocking");
    println!("• Metadata is cached to reduce syscalls");
    println!("• The view manages navigation and rendering state");
    println!("• Use FileExplorerRenderer::render() to display in a ratatui Frame");
    println!();
    println!("Actions available:");
    println!("- ToggleFileExplorer: Show/hide the file explorer");
    println!("- FileExplorerUp/Down: Navigate through the tree");
    println!("- FileExplorerExpand: Expand selected directory");
    println!("- FileExplorerCollapse: Collapse selected directory");
    println!("- FileExplorerOpen: Open selected file");
    println!("- FileExplorerRefresh: Refresh directory contents");

    Ok(())
}
