<h1 align="center"> 🗃️ SVGenerator </h1>

<p align="center">
  <strong>✨ SVG to Cairo Code Converter </strong>
</p>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

## ⚡ About SVGenerator ⚡

SVGenerator is a tool to convert SVG files to Cairo code that outputs the SVG file.
SVG groups `<g id="group1"> .. </g>` will produce a specific Cairo function

```rust
fn print_group1(ref svg, data: @Data) { .. }
```

The SVG can be annotated with `@@var_name@@` to allow data to be added dynamically in Cairo.

## 🛠️ Build

To build the project, run:

```bash
cargo build
```

## Usage

### Generate

To convert a (annotated) SVG file to Cairo:

```bash
cargo run --generate path/to/yourfile.svg
```

To escape quotation marks `".."`

```bash
cargo run -- generate path/to/yourfile.svg --escaped
cargo run -- generate path/to/yourfile.svg -e
```

### Groups

To visualize the SVG grouping structure:

```bash
cargo run -- groups path/to/yourfile.svg
```

## 📖 License

This project is licensed under the **APACHE 2 license**. See [LICENSE](LICENSE) for more information.
