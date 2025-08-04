tab0Server <- function(input, output, session) {
  #-HANDLER FOR INSTRUCTION DOWNLOAD--------------------------------------------
  output$download_inst <- downloadHandler(

    # Define download file name
    filename = function() {
      paste("instructions", Sys.Date(), ".pdf", sep = "")
    },

    # Define the content of the file to be downloaded
    content = function(file) {
      file.copy("www/fr-tab-instructions.pdf", file)
    }
  )
}
