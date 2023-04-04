! md_parameters.f -- parameters module for MD simulation

      MODULE PARAMETERS
      IMPLICIT NONE
      ! Number of atoms in system
      integer, parameter             :: np = 864
      ! Number of timesteps to run simulation
      integer, parameter             :: nsteps = 5000
      ! Atom name for output trajectory
      character (len = 2), parameter :: aname  = 'Ar'
      ! Lennard-Jones parameter sigma, nm
      real, parameter                :: sigma  = 0.34
      ! LJ cutoff, nm
      real, parameter                :: Rc      = 0.765 
      ! Lennard-Jones parameter epsilon, J
      real, parameter                :: eps    = 1.657E-21
      ! Box size, nm
      real, parameter                :: length    =  3.47786
      ! Mass of each atom, kg
      real, parameter                :: mass   = 6.6335209E-26
      ! Timestep for simulation, ps
      real, parameter                :: dt     = 1E-3
      ! Whether or not to use thermostat; .TRUE. or .FALSE.
      logical, parameter             :: Tcoupl = .TRUE.
      ! Reference temperature for thermostat
      real, parameter                :: Ref_T  = 94.4
      ! Frequency to write to trajectory and log files
      integer, parameter             :: stride = 25
      ! Name of input file for simulation
      character (len=*), parameter   :: inputfile = 'Ar_Lattice.gro'
      real, parameter                :: boltz_k= 1.38065E-23
      END MODULE
