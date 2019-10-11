#' manage hsds URL
#' @return URL of hsds server
#' @examples
#' URL_hsds()
#' @export
URL_hsds = function() {
  "http://hsdshdflab.hdfgroup.org"
}

#' An S4 class to represent a HDF5 server listening on a port.
#'
#' This class is deprecated and will be defunct in the next release.
#'
#' @slot endpoint URL for server
#' @slot type Type of server software at the source; must be
#' either 'h5serv' or (default) 'hsds'
setClass("HSDSSource", representation(endpoint="character", type="character"))

#' Construct an object of type HSDSSource.
#'
#' A HSDSSource is a representation of a URL which provides access to a HDF5
#' server (either h5serv or hsds.)
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name HSDSSource
#' @param endpoint URL for server
#' @param type Type of server software at the source; must be
#' @return An object of type HSDSSource
#' @examples
#' src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' @export
HSDSSource <- function(endpoint, type='hsds')  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  if (!(type %in% c('h5serv', 'hsds')))
    stop(paste("unknown server type ", type))
  obj <- new("HSDSSource", endpoint=endpoint, type=type)
  # member root id also?
}

.HSDSSource <- function(endpoint, type='hsds')  { # after deprecation cycle this private function is used
  if (!(type %in% c('h5serv', 'hsds')))
    stop(paste("unknown server type ", type))
  # obj <- new("HSDSSource", endpoint=endpoint, type=type) # should not use assignment as function value
  new("HSDSSource", endpoint=endpoint, type=type)
}

#' An S4 class to represent an HDF5 file accessible from a server.
#'
#' @slot HSDSSource an object of type HSDSSource
#' @slot domain the file's domain on the server; more or less, an alias for its
#' location in the external server file system
#' @slot dsetdf a data.frame that caches often-used information about the file
setClass("HSDSFile", representation(src="HSDSSource", domain="character", dsetdf="data.frame"))

#' Construct an object of type HSDSFile
#'
#' A HSDSFile is a representation of an HDF5 file the contents of which are accessible
#' exposed by a HDF5 server.
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name HSDSFile
#' @param src an object of type HSDSSource, the server which exposes the file
#' @param domain the domain string; the file's location on the server's
#' file system.
#' @return an initialized object of type HSDSFile
#' @examples
#' src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' f10x <- HSDSFile(src, '/shared/bioconductor/tenx_full.h5')
#' @export
HSDSFile <- function(src, domain)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  request <- paste0(src@endpoint, '?domain=', domain)
  response <- tryCatch(
    submitRequest(request),
    error=function(e) { NULL }
  )
  if (is.null(request))  {
    warning("no such file")
    return(NULL)
  }
  dsetdf <- findDatasets(src, domain)
  obj <- new("HSDSFile", src=src, domain=domain, dsetdf=dsetdf)
}

.HSDSFile <- function(src, domain)  {  # after deprecation cycle this private function will be used
  request <- paste0(src@endpoint, '?domain=', domain)
  response <- tryCatch(
    submitRequest(request),
    error=function(e) { NULL }
  )
  if (is.null(request))  {
    warning("no such file")
    return(NULL)
  }
  dsetdf <- findDatasets(src, domain)
  obj <- new("HSDSFile", src=src, domain=domain, dsetdf=dsetdf)
}

#' An S4 class to represent a dataset in a HDF5 file.
#' @import httr methods rjson
#' @slot file An object of type HSDSFile; the file in which the dataset is resident.
#' @slot path The dataset's path in the internal HDF5 hiearchy.
#' @slot uuid The unique unit ID by which the dataset is accessed in the server 
#' database system.
#' @slot shape The dimensions of the dataset
#' @slot type The dataset's HDF5 datatype
setClass("HSDSDataset", representation(file="HSDSFile", path="character", uuid="character",
                                       shape="numeric", type="list"))

#' Construct an object of type HSDSDataset 
#' A HSDSDataset is a representation of a dataset in a HDF5 file.
#' @name HSDSDataset
#' @param file An object of type HSDSFile which hosts the dataset 
#' @param path The complete intrafile path to the dataset
#' @return An initialized object of type HSDSDataset
#' @examples
#' src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' f <- HSDSFile(src, '/home/spollack/testzero.h5')
#' d <- HSDSDataset(f, '/grpA/grpAB/dsetX')
#' @export
HSDSDataset <- function(file, path)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  idx <- which(file@dsetdf[,1] == path)
  if (length(idx) == 0)  
    stop("no such dataset")
  uuid <- file@dsetdf[idx,2]
  request <- paste0(file@src@endpoint, '/datasets/', uuid, '?domain=', file@domain)
  response <- tryCatch(submitRequest(request),
                       error=function(e) { NULL })
  if (is.null(response))  { # this should be almost impossible
    warning("bad http request", call. = FALSE)
    return(NULL)
  }
  shape <- response$shape$dims
  type <- list(class=response$type$class, base=response$type$base)
  
  new("HSDSDataset", file=file, path=path, uuid=uuid,
      shape=shape, type=type)
}

.HSDSDataset <- function(file, path)  { # after deprecation cycle this is all that will remain
  idx <- which(file@dsetdf[,1] == path)
  if (length(idx) == 0)  
    stop("no such dataset")
  uuid <- file@dsetdf[idx,2]
  request <- paste0(file@src@endpoint, '/datasets/', uuid, '?domain=', file@domain)
  response <- tryCatch(submitRequest(request),
                       error=function(e) { NULL })
  if (is.null(response))  { # this should be almost impossible
    warning("bad http request", call. = FALSE)
    return(NULL)
  }
  shape <- response$shape$dims
  type <- list(class=response$type$class, base=response$type$base)
  
  new("HSDSDataset", file=file, path=path, uuid=uuid,
      shape=shape, type=type)
}


#  private - traverse internal file hiearchy, find datasets, and
#  cache often-accessed information in a data.frame for the HSDSFile object.
findDatasets <- function(src, domain)  {
  
  result <- tryCatch({
    request <- paste0(src@endpoint, '?domain=', domain)
    response <- submitRequest(request)
    fileroot <- response$root
    
    # ye olde depth-first search
    eee <- new.env(parent=emptyenv())
    eee$results <- c()            # paths to datasets
    eee$uuids <- c()
    
    search <- function(uuid, path, ee)  {
      # ee$results <- c(ee$results, path)
      request <- paste0(src@endpoint, '/groups/', uuid, '/links?domain=', domain)
      response <- submitRequest(request)
      for (link in response[['links']])  {
        if ('collection' %in% names(link) && link[['collection']] == 'groups')  {
          nxtuuid <- link[['id']]
          nxtpath <- paste0(path, '/', link[['title']])
          search(nxtuuid, nxtpath, ee)
        } else if ('collection' %in% names(link) && link[['collection']] == 'datasets')  {
          nxtuuid <- link[['id']]
          nxtpath <- paste0(path, '/', link[['title']])
          ee$results <- c(ee$results, nxtpath)
          ee$uuids <- c(ee$uuids, nxtuuid)
        }
      }
    }
    search(fileroot, '', eee)
    1
  }, error = function(e) { -1 })
  
  if (result == -1)  {
    warning(paste0("no datasets for file ", domain), call. = FALSE)
    return(data.frame(paths=c(), uuids = c(), stringsAsFactors = FALSE))
  }
  return(data.frame(paths=eee$results, uuids=eee$uuids, stringsAsFactors = FALSE))
}

# private - submit request and handle errors 
submitRequest <- function(req, transfermode='JSON')  {
  rsp <- tryCatch({
    if (transfermode == 'JSON')  {
      httr::GET(req)
    } else if (transfermode == 'binary')  {
      httr::GET(req, add_headers(Accept="application/octet-stream"))
    }
  }, error=function(e) { NULL }
  )
  
  if (is.null(rsp))  
    stop("Bad request")
  
  if ('status_code' %in% names(rsp) && rsp$status_code != 200)  
    stop("Bad request")
  
  if (transfermode == 'JSON')  {
    rsp <- tryCatch({
      rjson::fromJSON(readBin(rsp$content, what="character"))
    }, error=function(e) { NULL })
  }
  
  if (is.null(rsp))  
    stop("Bad request")
  
  # Note: rsp could be the empty string - not an error
  # maybe throw a warning condition?
  
  return(rsp)
  
}