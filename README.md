SENEX
=====
SENEX contains IDL scripts for creating, processing, and analysing aircraft data and GEOS-Chem output from the SENEX campaign.

You will need to include a line in your idl_startup.pro that defines the !SENEX system variable.

You will also need to keep your aircraft & model data in the outer SENEX directory in the following structure:
!SENEX
  gc_data/ -- model output
  field_data/
    WP3D/
      merge_60s/ -- one-minute merged aircraft data
      merge_10m_0.3125x0.25/ -- merged aircraft data averaged to GEOS-Chem resolution
      
These data are not included here as they are not currently publicly released.

A minor change is made by lei here to test GitHub.
