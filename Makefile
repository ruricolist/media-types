MIME_TYPES = https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types

mime.types:
	wget -N $(MIME_TYPES)
