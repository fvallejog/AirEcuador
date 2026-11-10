A synthetic workbook is included inside quito-synthetic.zip.
No monitoring observations have been copied from either provider.
Blank station headers, Excel date serials, signed temperatures and the final date are represented.

quito-date-cells.zip contains a separate synthetic workbook with Excel date-formatted cells and no unit header row, matching the layout observed in the official CO.rar supplied on 2026-10-27. No provider concentrations are included.

quito-stored-xlsx.rar contains the same synthetic workbook as PM2.5.xlsx.
The RAR5 was created with `rar a -m0 quito-stored-xlsx.rar PM2.5.xlsx`.
The stored member reproduces libarchive selecting the embedded XLSX ZIP
when the outer RAR is opened as a seekable file.
