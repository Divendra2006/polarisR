# polarisR: Non-Linear Dimensionality Reduction Visualization Tool

`polarisR` is a Shiny application for visualizing high-dimensional data using non-linear dimensionality reduction (NLDR) techniques such as t-SNE and UMAP. It provides an interactive platform to explore high-dimensional datasets, diagnose the quality of the embeddings using the `quollr` package, and compare different NLDR methods.

## Features

- **Data Loading**: Load your own CSV datasets or use one of the example datasets provided.
- **NLDR Methods**: Choose between t-SNE and UMAP for dimensionality reduction.
- **Interactive Visualization**: Explore the 2D embedding of your data with interactive plots.
- **Dynamic Tour**: Take a dynamic tour of the high-dimensional data to understand its structure.
- **Embedding Diagnostics**: Use the `quollr` package to diagnose the quality of the NLDR embedding.
- **Method Comparison**: Compare the results of different NLDR methods and parameter settings.
- **Side-by-Side Visualization**: View two visualizations side-by-side with linked brushing for a direct comparison.

## Installation

You can install polarisR from GitHub using the following commands in R:

```R
# Install pak if you haven't already
# install.packages("pak")

pak::pak("Divendra2006/polarisR")
```

## Usage

To run the polarisR Shiny app, use the following command in R:

```R
polarisR::run_nldr_viz()
```

## Datasets

polarisR comes with the following example datasets:

- `four_clusters`: A simple dataset with four distinct clusters.
- `pdfsense`: A dataset from high-energy physics, representing the space of parton distribution function fits.
- `trees`: A high-dimensional tree data structure with 10 branching points.

You can also upload your own dataset in CSV format.

## About the name

**p**rojective **o**utput **l**ayouts **a**nd **r**educed **i**nteractive **s**urfaces in **R**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
