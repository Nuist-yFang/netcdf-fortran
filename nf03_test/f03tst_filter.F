C     This is part of the netCDF package.
C     Copyright 2006 University Corporation for Atmospheric Research/Unidata.
C     See COPYRIGHT file for conditions of use.

C     This program tests netCDF-4 variable Filter support from fortran.

C     $Id: ftst_vars.F,v 1.19 2009/09/27 21:25:23 ed Exp $

      program ftst_vars
      USE netcdf4_f03
      implicit none

C     This is the name of the data file we will create.
      character*(*) FILE_NAME
      parameter (FILE_NAME='ftst_vars.nc')

C     We are writing 2D data, a 6 x 12 grid. 
      integer NDIMS
      parameter (NDIMS=2)
      integer NX, NY
      parameter (NX = 6, NY = 12)

C     NetCDF IDs.
      integer ncid, varid, dimids(NDIMS)
      integer x_dimid, y_dimid

C     This is the data array we will write, and a place to store it when
C     we read it back in.
      integer data_out(NY, NX), data_in(NY, NX)

C     For checking our data file to make sure it's correct.
      integer chunks(NDIMS), chunks_in(NDIMS)
      integer filterid, nparams, params(1)

C     Loop indexes, and error handling.
      integer x, y, retval

C     Create some pretend data.
      do x = 1, NX
         do y = 1, NY
            data_out(y, x) = 2147483646 + x * y
         end do
      end do

      print *, ''
      print *,'*** Testing definition netCDF-4 Filters'

C     Create the netCDF file.
      retval = nf_create(FILE_NAME, NF_CLOBBER+NF_NETCDF4, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Define the dimensions.
      retval = nf_def_dim(ncid, "x", NX, x_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, "y", NY, y_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Define the variable. 
      dimids(1) = y_dimid
      dimids(2) = x_dimid
      retval = nf_def_var(ncid, "data", NF_INT64, NDIMS, dimids, varid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Turn on chunking.
      chunks(1) = NY
      chunks(2) = NX
      retval = nf_def_var_chunking(ncid, varid, 0, chunks)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Set bzip filter on variable
      params(1) = 9
      retval = nf_def_var_filter(ncid, varid, 307, 1, params)
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Reopen the file and check again.
      retval = nf_open(FILE_NAME, NF_NOWRITE, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Find our variable.
      retval = nf_inq_varid(ncid, "data", varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (varid .ne. 1) stop 2

C     Check the filter
      params(1) = 9
      retval = nf_inq_var_filter(ncid, varid, filterid, nparams, params)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (filterid .ne. 307) stop 2
      if (nparams .ne. 1) stop 2
      if (params(1) .ne. 9) stop 2

C     Close the file. 
      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

      print *,'*** SUCCESS!'
      end
