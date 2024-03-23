theme_censusprofiler <- function(x,...){
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.", 
                 "theme_vanilla()"))
  }
  thick_b <- fp_border(width = 2, 
                     color = "#2D2A26")
  std_b <- fp_border(width = 1,
                      color = "#857C71")
  thin_b <- fp_border(width = .5,
                      color = "#857C71")
  x <- border_remove(x)
  x <- font(x, fontname = "Open Sans", part = "all")
  x <- fontsize(x, size = 9.5, part = "all")
  x <- color(x, color = "#2D2A26", part="header")
  x <- bg(x , bg="#998F82",part="header")
  x <- hline(x, border = fp_border(width=.5,color="#857C71"), part = "all")
  x <- border_outer(x, border=std_b, part="header")
  x <- border_inner_h(x, border=thin_b,)
  #x <- hline_top(x, border = thick_b, part = "header")
  x <- hline_bottom(x, border = std_b, part = "header")
  x <- hline_bottom(x, border = std_b, part = "body")
  x <- bold(x = x, bold = TRUE, part = "header")
  x <- padding(x, padding = 8, part="all")
  x <- align_text_col(x, align = "left", header = TRUE)
  x <- align_nottext_col(x, align = "right", header = TRUE)
  x <- autofit(x)
  fix_border_issues(x)
}
