INTERFACE zif_uitb_c_alv_colheader_mode
  PUBLIC .

  "! Description and tooltip are taken from data element
  CONSTANTS default TYPE i VALUE 1.
  "! Technical column name is taken as tooltip and description
  "! is taken as column header
  CONSTANTS tech_as_tooltip TYPE i VALUE 2.
  "! Technical column name is taken as column header and
  "! description is taken as tooltip
  CONSTANTS tech_as_column_header TYPE i VALUE 3.
ENDINTERFACE.
