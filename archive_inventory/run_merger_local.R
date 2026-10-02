#test shiny/scheduled_scripts/archive_merger.R locally without RMariaDB:
#database reads go through the mysql CLI against the local hbef database.
#writes shiny/HTML/archive_explore/archive_data.js (gitignored) and
#output/archive_samples.csv here. the local database may lag the server's.
#usage: Rscript run_merger_local.R

out = normalizePath('~/git/hbef/hbef_misc/archive_inventory/output')
mysql_pass = readLines('~/git/hbef/RMySQL.config')

mysql = function(query, f){
    system(sprintf('mysql -u root -p"%s" hbef -B -e "%s" > %s 2>/dev/null',
                   mysql_pass, query, f))
}

src = readLines('~/git/hbef/shiny/scheduled_scripts/archive_merger.R')
src = sub("^setwd\\('/home/mike/shiny/'\\).*", "setwd('~/git/hbef/shiny/'); edi_dir = out", src)
src = src[! grepl('^library\\(RMariaDB\\)|^if\\(edi_dir == ', src)]

dbConnect = function(...) NULL
dbDisconnect = function(...) invisible(TRUE)
dbReadTable = function(con, tbl){

    f = tempfile(fileext = '.tsv')
    mysql(paste('select * from', tbl), f)
    d = readr::read_tsv(f, na = 'NULL', col_types = readr::cols(.default = 'c'))

    #type columns as RMariaDB would
    ft = tempfile(fileext = '.tsv')
    mysql(sprintf("select column_name, data_type from information_schema.columns where table_schema='hbef' and table_name='%s'", tbl), ft)
    ty = read.delim(ft)
    for(i in seq_len(nrow(ty))){
        cl = ty[i, 1]
        if(ty[i, 2] %in% c('decimal', 'int', 'smallint', 'year', 'float', 'double', 'tinyint')) d[[cl]] = as.numeric(d[[cl]])
        if(ty[i, 2] == 'date') d[[cl]] = as.Date(d[[cl]])
    }

    as.data.frame(d)
}

eval(parse(text = src))
