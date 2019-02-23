"! <p class="shorttext synchronized" lang="en">Table</p>
INTERFACE zif_uitb_table
  PUBLIC .


  "! <p class="shorttext synchronized" lang="en">Adds new line to the table</p>
  METHODS add_line
    IMPORTING
      !if_insert          TYPE boolean OPTIONAL
    RETURNING
      VALUE(rv_new_index) TYPE sy-tabix .
  "! <p class="shorttext synchronized" lang="en">Updates the values of the current line in the table</p>
  METHODS update_fields
    IMPORTING
      !iv_function_code TYPE sy-ucomm OPTIONAL .
  "! <p class="shorttext synchronized" lang="en">Update the screen attributes of the line</p>
  METHODS update_screen_attributes .
  "! <p class="shorttext synchronized" lang="en">Determines the line count</p>
  METHODS determine_line_count .
  "! <p class="shorttext synchronized" lang="en">Determines the current line</p>
  METHODS determine_current_line .
  "! <p class="shorttext synchronized" lang="en">Retrieves the current line index</p>
  METHODS get_current_line_index
    RETURNING
      VALUE(rv_index) TYPE sy-tabix .
  "! <p class="shorttext synchronized" lang="en">Deletes the current line</p>
  METHODS delete_current_line .
  "! <p class="shorttext synchronized" lang="en">Gets the value of the current line</p>
  METHODS get_current_line_value DEFAULT IGNORE
    EXPORTING
      !es_line TYPE any .
  "! <p class="shorttext synchronized" lang="en">Gets a reference to the current line</p>
  METHODS get_current_line_ref DEFAULT IGNORE
    RETURNING
      VALUE(rr_line) TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">Deletes all rows</p>
  METHODS delete_all .
  "! <p class="shorttext synchronized" lang="en">Gets the current loop line</p>
  METHODS get_current_loop_line DEFAULT IGNORE
    RETURNING
      VALUE(rv_current_loop_line) TYPE sy-tabix .
  "! <p class="shorttext synchronized" lang="en">Get all table data</p>
  METHODS get_table_data DEFAULT IGNORE
    RETURNING
      VALUE(rr_table_data_itab) TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">PBO for each line of the table</p>
  METHODS pbo DEFAULT IGNORE .
ENDINTERFACE.
