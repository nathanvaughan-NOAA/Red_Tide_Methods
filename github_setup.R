# R code to connect github to a Google Cloud Workstation

# Go to Github and generate a Personal Access Token (PAT) with the permissions 
# you need:  
# https://github.com/settings/tokens
# Github tutorial here: 
# https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens

# ctrl+f and replace USER_NAME with your username
# ctrl+f and replace E_MAIL with your email address
# ctrl+f and replace TOKEN with your PAT Token

system('git config --global user.name "USER_NAME"')
system('git config --global user.email "E_MAIL"')
system("git config --list")  #check that the info is correct

cred_line <- "https://USER_NAME:TOKEN@github.com"
writeLines(cred_line, "~/.git-credentials")
system('git config --global credential.helper store')

system('git clone https://github.com/SEFSC/SEFSC-SFD-Episodic_Mortality_Manuscript.git')
