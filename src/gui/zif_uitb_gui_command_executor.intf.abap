"! <p class="shorttext synchronized" lang="en">Executable GUI Command</p>
INTERFACE zif_uitb_gui_command_executor
  PUBLIC .
  CONSTANTS c_ucomm_prefix TYPE ui_func VALUE '_&EXC_'.

  "! <p class="shorttext synchronized" lang="en">Executes the command logic</p>
  "!
  METHODS execute.
ENDINTERFACE.
