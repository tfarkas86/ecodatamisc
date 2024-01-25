#' List names of files within a bucket on AWS S3
#'
#' This function is a wrapper for functionality of paws list_objects. It parses list format returned by paws to return only file names in a given directory.
#' @param aws_profile Name of profile to access AWS. This must match .aws config file
#' @param bucket_name Name of AWS bucket
#' @param sub_dir Name of subdirectory within specified AWS bucket (optional)
#'
#' @return A character vector of paths to files in specified s3 bucket and sub directory
#' @export
#' @import paws
#'
s3_listfiles <- function(aws_profile, bucket_name, sub_dir="") {

  # specify AWS profile to use
  Sys.setenv(AWS_PROFILE = aws_profile)

  # set AWS service to use
  s3 <- paws::s3()

  # store list of top level directories in S3
  objs <- s3$list_objects(bucket_name,
                          Delimiter= '/',
                          Prefix='')

  # loop through directories
  # print file names that are within directories that match specified sub_dir
  for (prefix in objs$CommonPrefixes) {
    # store folder name in object
    folder <- prefix$Prefix

    # store response as list
    resp <- s3$list_objects(
      Bucket=bucket_name,
      Prefix= paste0(folder, sub_dir),
    )

    if (length(resp$Contents) != 0) {
      for (obj in resp$Contents) {

        if (obj$ETag == resp$Contents[[1]]$ETag) {
          files <- obj$Key
        } else {
          files <- c(files, obj$Key)
        }
      }
      return(files)
    }

  }
}
