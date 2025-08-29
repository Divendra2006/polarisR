#' PDFSense Dataset
#'
#' A dataset representing the space of parton distribution function fit, each point in the variables
#' labelled X1-X56 indicate moving +- 1 standard devation from the 'best'
#' (maximum likelihood estimate) fit of the function. Each observation has
#' all predictions of the corresponding measurement from an experiment.
#'
#' (see table 3 in that paper for more explicit details).
#'
#' The remaining columns are:
#'
#' * InFit: A flag indicating whether an observation entered the fit of
#'   CT14HERA2 parton distribution function
#' * Type: First number of ID
#' * ID: contains the identifier of experiment, 1XX/2XX/5XX correpsonds
#' to Deep Inelastic Scattering (DIS) / Vector Boson Production (VBP) /
#'  Strong Interaction (JET). Every ID points to an experimental paper.
#' * pt: the per experiment observational id
#' * x,mu: the kinematics of a parton. x is the parton momentum fraction, and
#' mu is the factorisation scale.
#'
#' @references
#' Wang, B.-T., Hobbs, T. J., Doyle, S., Gao, J., Hou, T.-J., Nadolsky, P. M.,
#' & Olness, F. I. (2018). PDFSense: Mapping the sensitivity of
#' hadronic experiments to nucleon structure.
#' Retrieved from [https://arxiv.org/abs/1808.07470](https://arxiv.org/abs/1808.07470)
#'
#' Cook, D., Laa, U., & Valencia, G. (2018).
#' Dynamical projections for the visualization of PDFSense data.
#' The European Physical Journal C, 78(9), 742.
#' \doi{10.1140/epjc/s10052-018-6205-2}
#'
#'
#' @source [http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/](http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/)
"pdfsense"

#' A high-dimensional tree data structure with 10 branching points.
#'
#' @description Data are obtained from diffusion limited aggregation
#' tree simulation in the `phate` python and `phateR` packages, but
#' reconstructed as a wide data.frame rather than a list.
#'
#' There are 3000 rows and 101 columns, the first 100 columns are labelled
#' dim1 - dim100, and are numeric, while the final column is a
#' factor representing the branch id.
#'
#' @source [PHATE](https://github.com/KrishnaswamyLab/PHATE/blob/master/Python/phate/tree.py)
"fake_trees"


#' Four Clusters Simulated Dataset
#'
#' @title Four Clusters Simulated Dataset
#' @description This dataset (`four_clusters`) contains simulated data with four distinct clusters,
#' each generated using different shapes and scales. It is ideal for demonstrating
#' clustering algorithms and visualization techniques.
#'
#' @format A data frame with 2000 rows and 4 variables:
#' \describe{
#'   \item{x1}{Numeric. First feature coordinate.}
#'   \item{x2}{Numeric. Second feature coordinate.}
#'   \item{x3}{Numeric. Third feature coordinate.}
#'   \item{x4}{Numeric. Fourth feature coordinate.}
#'   \item{cluster}{Factor. The cluster label (1, 2, 3, or 4).}
#' }
#'
#' @source Simulated using \code{cardinalR} package.
#'
#' @references
#' Gamage J, Cook D, Harrison P, Lydeamore M, Talagala T (2025)._cardinalR:
#' Collection of Data Structures_. R package version 0.1.10,
#' <https://github.com/JayaniLakshika/cardinalR>.
#'
#' @examples
#' data(four_clusters)
#' head(four_clusters)
#' dim(four_clusters)
"four_clusters"
