commit = function()
{
    code = shell('C: & cd C:/Users/vpran/Desktop & cd "C:/Users/vpran/Documents/GitHub/Intro-to-R" & git.exe pull & git.exe add . & git.exe commit -m "comments" & git.exe push origin master')
    cat("\014")
    
}

