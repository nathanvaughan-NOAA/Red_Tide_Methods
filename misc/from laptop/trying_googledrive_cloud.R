library(googledrive)

# 1. meaningful name for the hidden secrets folder
# This tells R to store the credentials in this specific project folder
options(gargle_oauth_cache = ".secrets")

# 2. Trigger the auth
# When the browser opens, sign in with the account you want to use.
# Make sure to check the box to grant access to Drive files.
drive_auth()

# 3. Verify it worked
drive_find(n_max = 5)


library(googledrive)

# 1. Set the cache folder again
options(gargle_oauth_cache = ".secrets")

# 2. Authenticate with EXPLICIT scopes
# This forces the login window to ask for full Drive access
drive_auth(
  email = "",
  scopes = "https://www.googleapis.com/auth/drive"
)
