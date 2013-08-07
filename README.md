SENEX-IDL
=========
SENEX contains IDL scripts for creating, processing, and analysing aircraft data and GEOS-Chem output from the SENEX campaign.

You will need to include a line in your idl_startup.pro that defines the !SENEX system variable.
This is done using the following syntax:
SENEX = '/home/username/yourpath/SENEX'
DefSysV, '!SENEX',Exists=Exists
if (not Exists ) then DefSysV,'!SENEX',SENEX

You may also want to add this path to the default IDL search path using
Pref_Set, 'IDL_PATH', Expand_Path('+!SENEX/IDL/', /All_Dirs ) + SEP + '<IDL_DEFAULT>', /Commit

You will also need to keep your aircraft & model data in the outer SENEX directory in the following structure:
!SENEX
  gc_data/ -- model output
  field_data/
    WP3D/
      merge_60s/ -- one-minute merged aircraft data
      merge_10m_0.25x0.3125/ -- merged aircraft data averaged to GEOS-Chem resolution
      
These data are not included here as they are not currently publicly released.

For questions, contact Jenny Fisher (jennyf@uow.edu.au) and Lei Zhu (leizhu@fas.harvard.edu)
