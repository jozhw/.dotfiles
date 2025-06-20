---
title: CLI Tools
description: Summary of configs for cli tools used
---

This file contains the configurations of cli tools used, if not already specifed in the configuration file, for quick reference.

## Wget

The following was generated using ChatGPT-3.5:

This `.wgetrc` file contains various configuration options for the wget command-line utility. 

### Current Settings

1. `timestamping = on`: Enables timestamping, which means wget will use the server-provided last modification date if available. This helps in efficiently downloading only the changed or updated files.

2. `no_parent = on`: Prevents wget from going up in the directory structure when downloading recursively. It restricts downloading to the specified directory and its subdirectories.

3. `timeout = 60`: Sets the timeout for DNS, connect, and read operations to 60 seconds. If a connection or read operation takes longer than this timeout, wget will give up and move on to the next task.

4. `tries = 3`: Specifies the number of times wget should retry a download when it fails. In this case, it's set to retry the download up to 3 times before giving up.

5. `retry_connrefused = on`: Configures wget to retry even when the connection was refused by the server.

6. `trust_server_names = on`: Uses the last component of a redirection URL for the local file name. This ensures that the filename saved locally corresponds to the filename provided by the server.

7. `follow_ftp = on`: Enables wget to follow FTP links from HTML documents by default.

8. `adjust_extension = on`: Adds a `.html` extension to `text/html` or `application/xhtml+xml` files and a `.css` extension to `text/css` files if they lack one. This ensures proper file extensions for downloaded files.

9. `robots = off`: Disables wget from obeying the `robots.txt` file or `<meta name=robots content=nofollow>` tag, which specifies rules for web crawlers.

10. `server_response = on`: Instructs wget to print the HTTP and FTP server responses. This can be helpful for debugging and understanding server behavior.

11. `user_agent = Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)`: Sets the User-Agent string to disguise wget as Internet Explorer 9 on Windows 7. This can be useful for accessing websites that serve different content based on the user-agent.

