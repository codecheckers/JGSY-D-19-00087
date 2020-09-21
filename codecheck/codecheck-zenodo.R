## Zenodo deposit; see vignette("codecheck_overview.Rmd")

library("codecheck")

metadata = yaml::read_yaml( "codecheck.yml")
my_token = zenodoToken

zenodo <- zen4R::ZenodoManager$new(token = my_token)

# record was manually created
record = get_zenodo_record(metadata$report)
codecheck:::set_zenodo_metadata(zenodo, record, metadata)

## If you have already uploaded the certificate once, you will need to
## delete it via the web page before uploading it again.
## codecheck:::set_zenodo_certificate(zenodo, record, "codecheck.pdf")

codecheck:::set_zenodo_certificate(zenodo, record, "codecheck/codecheck.pdf")
# you can confirm the file validity by running md5sum codecheck.pdf locally

## You may also create a ZIP archive of of any data or code files that
## you think should be included in the CODECHECK's record.
zip::zip("JGSY-D-19-00087.zip", list.files(".", recursive = TRUE, all.files = TRUE), recurse = TRUE)

zenodo$uploadFile("JGSY-D-19-00087.zip", record)

## Now go to zenodo and check the record, add a link 
