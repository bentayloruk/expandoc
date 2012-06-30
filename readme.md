# Expandoc

Creates a static documentation site using [Pandoc's](http://johnmacfarlane.net/pandoc/) HTML output.

This project is early doors.  I'm going to use it for the new [Enticify website](http://www.enticify.com/) *which is currently not live*.  


# Usage

*To early to be used :)*

## The Transformation Process

*  For each file in and below the **documents in path** (`pdi` switch).
*  Skip any files in or below directories beginning with "_"
*  Build relative `outputPath` to **documents out path** (`pdo`).   
Uses `numberWang` to go from `c:\doc\00-intro\10-text.md` to `c:\outdoc\intro\text.html`
*  If file is one we convert with Pandoc then
    *  Read file front matter if present.
    * Pass to Pandoc to get HTML.
    * Save HTML output to `outputPath` 
*  Else 
    * Copy file to `outputPath`.

## Make Contact

Chat to me [@bentayloruk](http://twitter.com/#!/bentayloruk)
