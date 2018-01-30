ReadNSM <- function(FilenameWithoutExt, InDirectory = getwd()) {
  #  Schreiben einer Named Sparse Matrix(NSM): i.e sparse Matrix mit  mit Zeilen- und Spaltennamen sowie Zeilen und Spalten Keys
  # NamedMatrix = ReadNSM(FileNameWithoutExt,InDirectory);  # lesen einer Sparse Matrix
  #
  # INPUT
  # FileNameWithoutExt                 Dateiname ohne extension
  #
  # OPTIONAL
  # InDirectory                        Directory aus dem gelesen wird
  #
  # OUTPUT
  # Matrix(1:l,1:c)                    Matrix mit l zeilen und c Spalten in sparse Darstellun
  #                                    (MATLAB sparse/full)  R: see Matrix package
  # LineKey(1:l)                       Schluessel fuer die Zeilen
  # LineHeader(1:l,:)                  Char Array mit Bezeichnungen fuer die Zeilen der Matrix
  # ColumnKey(1:c)                     Schluessel für die Spalten der Matrix
  # ColumnHeader(1:c,:)                Char Array mit Bezeichnungen fuer die Spalten der Matrix
  #
  # AUTHOR
  # FP 06/16
  
  # Step 0: Check if files exist
  colexist = T
  if (!file.exists(paste0(InDirectory, '/', FilenameWithoutExt, 'NSMmat.lrn')))
    stop("lrn file not found")
  if (!file.exists(paste0(InDirectory, '/', FilenameWithoutExt, 'NSMrow.names')))
    stop("row names not found")
  if (!file.exists(paste0(InDirectory, '/', FilenameWithoutExt, 'NSMcol.names'))) {
    warning("No column names found. Assuming matrix to be symmetric.")
    colexist = F
  }
  
  # Step 1: read Lrn
  lrn = ReadLRN(FileName = paste0(FilenameWithoutExt, 'NSMmat.lrn'),
                InDirectory = InDirectory)
  data = lrn$Data
  
  # Step 2: read names
  if (colexist) {
    cnames = ReadNAMES(
      FileName = paste0(FilenameWithoutExt, 'NSMcol.names'),
      InDirectory = InDirectory
    )
  }
  rnames = ReadNAMES(
    FileName = paste0(FilenameWithoutExt, 'NSMrow.names'),
    InDirectory = InDirectory
  )
  
  # Step 3: get dimensions of sparse matrix
  if (colexist) {
    dims = c(length(rnames$Key), length(cnames$Key))
  } else {
    dims = c(length(rnames$Key), length(rnames$Key))
  }
  # Step 4: reconstruct sparse matrix
  mat = Matrix::sparseMatrix(
    i = data[, 1],
    j = data[, 2],
    x = data[, 3],
    dims = dims
  )
  
  # Step 5: Output as List
  if (colexist) {
    return(
      list(
        matrix = mat,
        LineKey = rnames$Key,
        LineHeader = rnames$Names,
        ColumnKey = cnames$Key,
        ColumnHeader = cnames$Names,
        Comments = lrn$Comments
      )
    )
  } else {
    return(
      list(
        matrix = mat,
        LineKey = rnames$Key,
        LineHeader = rnames$Names,
        ColumnKey = rnames$Key,
        ColumnHeader = rnames$Names,
        Comments = lrn$Comments
      )
    )
  }
}