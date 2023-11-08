count_authors = function(bibname)
{   
    library(RefManageR)
    mybib <- RefManageR::ReadBib(bibname, check = FALSE)
    author_list = c(0,0,0)
    for (i in 1:length(mybib)) {
        paper = mybib[i]
        for (j in 1:length(paper$author)) {
            if (length (paper$author[j]$family) > 1) {next}
            else if (paper$author[j]$family == "Alves")
            {
                if (j==1){author_list[1]=author_list[1]+1}
                else if (j>1&j<length(paper$author)){author_list[2]=author_list[2]+1}
                else {author_list[3]=author_list[3]+1}
            }
        }
    }
    return(author_list)
}