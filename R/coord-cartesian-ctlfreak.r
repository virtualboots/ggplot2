#' Control freak version of cartesian coordinates
#'
#' This is an enhanced version of the original cartesian coordinate. It allows
#' specifying the x and y limit of every panel.
#'
#' @export
#' @inheritParams coord_cartesian
#' @examples
#' # The way to use it is specifying the xlim and ylim with a two-column matrix,
#' # respectively. The two columns of a row means the range. A row corresponds
#' # to a panel. Currently, due to API restriction, the mapping of a row to a
#' # facet is by panel index, which the user cannot know beforehand.
#'
#' p <- ggplot(mtcars, aes(disp, wt)) +
#'   geom_point() +
#'   geom_smooth() +
#'   facet_grid(cyl ~ .) +
#'   coord_cartesian_ctlfreak(ylim=matrix(c(0, 4, 2, 4, 3, 4), ncol = 2, byrow=TRUE))
coord_cartesian_ctlfreak <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                            default = FALSE) {
  ggproto(NULL, CoordCartesianCtlfreak,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCartesianCtlfreak <- ggproto("CoordCartesianCtlfreak", CoordCartesian,

  panel_idx = 0,

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    self$panel_idx <- self$panel_idx + 1
    
    train_cartesian <- function(scale, limits, name) {
      range <- scale_range(scale, limits, self$expand)

      out <- scale$break_info(range)
      out$arrange <- scale$axis_order()
      names(out) <- paste(name, names(out), sep = ".")
      out
    }
    c(
      train_cartesian(scale_x, self$limits$x[self$panel_idx, ], "x"),
      train_cartesian(scale_y, self$limits$y[self$panel_idx, ], "y")                               
    )
  }
)

scale_range <- function(scale, limits = NULL, expand = TRUE) {
  expansion <- if (expand) expand_default(scale) else c(0, 0)

  if (is.null(limits)) {
    scale$dimension(expansion)
  } else {
    range <- range(scale$transform(limits))
    expand_range(range, expansion[1], expansion[2])
  }
}
