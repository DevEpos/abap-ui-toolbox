"! <p class="shorttext synchronized" lang="en">View</p>
INTERFACE zif_uitb_view
  PUBLIC .

  DATA mf_visible TYPE abap_bool .

  "! <p class="shorttext synchronized" lang="en">Displays the view</p>
  "!
  "! If the control is a Non modal dialog the metric value is in
  "! Pixel. Also the additional parameter <strong>if_center</strong> is only
  "! valid for a non modal dialog and automatically centers the dialog on the
  "! current screen
  METHODS show
    IMPORTING
      if_center        TYPE abap_bool OPTIONAL
      !iv_start_line   TYPE i OPTIONAL
      !iv_start_column TYPE i OPTIONAL
      !iv_end_line     TYPE i OPTIONAL
      !iv_end_column   TYPE i OPTIONAL .
  "! <p class="shorttext synchronized" lang="en">Is this view visible</p>
  "!
  METHODS is_visible DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE abap_bool .
  "! <p class="shorttext synchronized" lang="en">Hides the view</p>
  "!
  METHODS hide DEFAULT IGNORE .
  "! <p class="shorttext synchronized" lang="en">Free allocated resources</p>
  "!
  METHODS free DEFAULT IGNORE .
ENDINTERFACE.
