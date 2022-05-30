library(aws.s3)

fnListBuckets <- function(){
    aws.s3::bucketlist()
}

# Create the bucket if it does not exist
fnCreateBucket <- function(bucket){
    if(!suppressMessages(bucket_exists(bucket))){
        put_bucket(bucket)
    }
}

fnCreatePrefix <- function(prefix, bucket){
    if(!suppressMessages(object_exists(prefix, bucket))){ 
        put_folder(prefix, bucket=bucket)
    }
}

fnPutFile <- function(inFilepath, s3Name, bucket, prefix){
    fullS3Path <- paste(prefix, s3Name, sep = "/")
    put_object(inFilepath, object=fullS3Path, bucket=bucket)
}

fnGetData <- function(bucket, prefix, s3Name){
    fullS3Path <- paste(prefix, s3Name, sep = "/")
    get_object(object=fullS3Path, bucket=bucket, as="text")
}

fnGetFile <- function(bucket, prefix, s3Name, outFilepath){
    fullS3Path <- paste(prefix, s3Name, sep = "/")
    if(object_exists(fullS3Path, bucket)){
        save_object(object=fullS3Path, bucket=bucket, file=outFilepath)
    }
    else{
        print(paste("Error: S3 object not found:", fullS3Path, " in bucket:", bucket))
    }
}





