# Accessing S3
# Before we can do anything, we need a globally accessible data store. There
# are 2 options. We can do either AWS S3 or use MiniO. This snippet allows us to
# initialize credentials/env so we can access.

fnConnectToS3Manual <- function(){
    # This should correspond to the endpoint URL (local.datascape.org or s3.amazonaws.com)
    Sys.setenv(AWS_S3_ENDPOINT=readline(prompt="S3 endpoint:"))
    # This is the "region" (s3.local or us-east-1)
    Sys.setenv(AWS_DEFAULT_REGION=readline(prompt="S3 region:"))
    # Access key or admin account
    Sys.setenv(AWS_ACCESS_KEY_ID=readline(prompt = "Access key:"))
    # Secret associated with the access key
    Sys.setenv(AWS_SECRET_ACCESS_KEY=readline(prompt = "Secret key:"))
}