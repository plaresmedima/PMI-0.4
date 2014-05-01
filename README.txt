PMI-0.4
=======

Source code for the Platform for Research in Medical Imgaging, version 0.4.

For more details see: https://sites.google.com/site/plaresmedima/

--------------------------------------------------------------------

Author : Steven Sourbron 

Email  : s.sourbron@leeds.ac.uk

Wepage: http://medhealth.leeds.ac.uk/profile/500/407/steven_sourbron


---------------------------------------------------------------------


The PMI source code is poorly documented, but this will get you started:


To run PMI from the source code
------------------------------- 

    - open IDL
    - add the folder "/PMI-0.4" to your IDL path
    - type "pmi" in the command line

To create a runtime version of PMI
---------------------------------- 

    - open IDL
    - type "pmi__compile" in the command line
    - save in the folder "/PMI-0.4-runtime-Skeleton"

To run the runtime version
-------------------------- 

    - double-click on the file "/PMI-0.4-runtime-Skeleton/pmi.sav" 
      (no need to open IDL first)

To run or compile PMI with a different menu (eg. "MyMenu")
----------------------------------------------------------

    - open the file "/PMI-0.4/Skeleton/PMI__Menu.pro" 
    - replace "PMI__Menu__Skeleton" by "PMI__Menu__MyMenu"
    - compile PMI__Menu
    - type "pmi" or "pmi__compile" in the command line



