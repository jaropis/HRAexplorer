#' CONSTANTS

# list holding the colors for selection
#' @export
glob_color_list <- list("orange" = "orange",
                 "white"="white",
                 "gray" = "gray",
                 "red"="#CC6666",
                 "violet"="#9999CC",
                 "green"="#66CC99",
                 "black" = "black")

#' function returning Poincare plot description
#'
#' @return HTML
#' @export
PP_description <- function(){
  return("Application for calculating Poincare plots descriptors for one or more files.
         <ul>
         <li> <strong>file format</strong> - The application requires either a <strong>text</strong> or <strong>Excel</strong> file (files), with at least the column containing the RR intervals. For text file any extension will be OK, for Excel you must use .xlsx (the latest verion). If you want to use the Excel file format, you must check the <strong>using Excel</strong> checkbox.
         <li> <strong>column selection</strong> - Enter the number of the column containing the RR intervals in the appropriate window. If you have flags, enter the number of the column holding them after a space or a comma. The 0 flag means <em>correct</em>, or of sinus origin. Select the separator (what you have beteen the columns in the text file) from the drop down menu.
         <li> <strong>time based filtering</strong> - Enter values to filter the RR intervals based on time. The first value sets the RR intervals that you consider too short to be of sinus origin, the second ones selects the intervals you consider too long to be of sinus origin.
         <li> Poincare <strong>plot</strong> is created for the first file on the list. If you want a plot for a specific file enter it as a single file for the analysis.
         </ul>")
}
