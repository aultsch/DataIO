WriteNSM <-
  function(FilenameWithoutExt,
           Matrix,
           LineKey,
           LineHeader,
           ColumnKey,
           ColumnHeader,
           OutDirectory = getwd(),
           Comments) {
    # Schreiben einer Named Sparse Matrix(NSM): i.e sparse Matrix mit  mit Zeilen- und Spaltennamen sowie Zeilen und Spalten Keys
    # WriteNSM(FileNameWithoutExt,Matrix); # schreiben einer spaerlichen Matrix
    # WriteNSM(FileNameWithoutExt,Matrix,LineKey,LineHeader,ColumnHeader,ColumnKey, OutDirectory);
    #
    # INPUT
    # FileNameWithoutExt                 Dateiname ohne extension
    # Matrix(1:l,1:c)                    Matrix mit l zeilen und c Spalten in sparse Darstellun
    #                                    (MATLAB sparse/full)  R: see Matrix package
    # OPTIONAL
    # OutDirectory                       Directory in das geschrieben wird,                       default: current directory
    # LineKey(1:l)                       Schluessel fuer die Zeilen,                              default: [1:l]
    # LineHeader(1:l,:)                  Char Array mit Bezeichnungen fuer die Zeilen der Matrix, default: []
    # ColumnKey(1:c)                     Schluessel für die Spalten der Matrix                    default: [1:c]
    # ColumnHeader(1:c,:)                Char Array mit Bezeichnungen fuer die Spalten der Matrix default: []
    # Comments                           Kommentarfeld was an WriteLRN weitergegeben wird. 
    #
    # AUTHOR
    # FP 06/16
    
    
    # Struktur der zu speichernden Daten:
    # Matrix                  -> Filenamewithoutext.NSMmat.lrn
    # Linekey, Lineheader     -> Filenamewithoutext.NSMrow.names
    # ColumnKey, CloumnHeader -> Filenamewithoutext.NSMcol.names
    
    
    # Step 1: Prepare the matrix
    if (attributes(class(Matrix))$package != "Matrix")
      stop(
        "Input Matrix is not out of package 'Matrix'. Input MUST be a sparse matrix from that package"
      )
    if (length(grep('ng*', class(Matrix)[1])) > 0) {
      stop("Sprase matrix with bool values given. These are not (yet) supported.")
    }
    # mc = class(Matrix)[1]
    # if(mc != "dgTMatrix")
    #   if(mc == "dgRMatrix" || mc == "dgCMatrix"){
    #     Matrix <- as(Matrix, 'dgTMatrix')
    #   } else {
    #     stop("No valid input matrix given. Must be sparse matrix from package Matrix. Types dgTMatrix, dgCMatrix and dgTMatrix accepted.")
    #   }
    #   # If you use this approch, remember to access Matrix with @. Matrix@i for example.
    #   # Also remember, that i and j are then offsetted by -1 (they are 0 indexed)
    # Alternative: Dont use this class thing at all. Use summary:
    s = Matrix::summary(Matrix) # Works for every type of sparse matrix. Works even for non sparse ones
    d = dim(Matrix)
    len = length(s$i)
    savematrix = matrix(nrow = len, ncol = 3)
    savematrix[, 1] = s$i
    savematrix[, 2] = s$j
    #mc = class(Matrix)[1]
    #if(mc == "ngCMatrix" || mc == 'ngTMatrix' || mc == 'ngRMatrix' || 'ngeMatrix')
    #  s$x = rep(T, length(s$i))
    savematrix[, 3] = s$x
    
    # Step 1.5: Some checks and fill-ins
    if (is.numeric(s$x))
      if (!(all(as.numeric(formatC(s$x, digits = 14)) == s$x)))
        warning(
          "Due to lrn format restrictions you might loose floating point precision. Precision of floating point numbers is limited to 13 digits"
        )
    if (missing(Comments))
      Comments <- ""
    if (missing(LineKey))
      LineKey <- 1:d[1]
    if (missing(LineHeader))
      LineHeader <- paste0("R", LineKey)
    missingcols = F
    if ((missing(ColumnHeader) || all(ColumnHeader == LineHeader)) &&
        (missing(ColumnKey) || all(ColumnKey == LineKey)) && d[1] == d[2]) {
      # If specific column information is missing
      # and the matrix has equal dimensions
      # we assume the headers and keys to be symmetric
      missingcols = T
    } else {
      if (missing(ColumnKey))
        ColumnKey <- 1:d[2]
      if (missing(ColumnHeader))
        ColumnHeader <- paste0("C", ColumnKey)
      
    }
    
    
    if (length(LineHeader) != d[1])
      stop('Wrong length: LineHeader')
    if (length(LineKey) != d[1])
      stop('Wrong length: LineKey')
    if (!missingcols) {
      if (length(ColumnHeader) != d[2])
        stop('Wrong lengt(h: ColumnHeader')
      if (length(ColumnKey) != d[2])
        stop('Wrong length: ColumnKey')
      
      WriteNAMES(
        FileName = paste0(FilenameWithoutExt, 'NSMcol.names'),
        Names = ColumnHeader,
        Key = ColumnKey,
        OutDirectory = OutDirectory
      )
    }
    
    # Step 2: Write everything to disk
    
    WriteLRN(
      FileName = paste0(FilenameWithoutExt, 'NSMmat.lrn'),
      Data = savematrix,
      OutDirectory = OutDirectory,
      Key = 1:len,
      CommentOrDigits = Comments
    )
    
    WriteNAMES(
      FileName = paste0(FilenameWithoutExt, 'NSMrow.names'),
      Names = LineHeader,
      Key = LineKey,
      OutDirectory = OutDirectory
    )
  }
