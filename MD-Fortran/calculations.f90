! md_calculations.f -- contains subroutines for MD simulation - position, velocity, forces, 
! kinetic, potential & total energies, temperature, pressure, diffusion and virial coefficients

! List of all variables
! pos          ::          position, nm 
! v            ::          velocity, nm/s 
! forces       ::          forces, N 
! PE           ::          Potential energy, J 
! KE           ::          Kinetic energy, J 
! np           ::          number of particles
! I            ::          components i,j 
! J            ::          iterations from 1 to np 
! K            ::          dimensions x,y,z
! r_cent       ::          distance between the centres of two particles, nm 
! length       ::          box length, nm
! sys_pres     ::          system pressure, Pa 
! sys_temp     ::          system temperature, K 
! acc          ::          acceleration, nm/s2
! old_vel      ::          previous velocity (from velocity verlet algorithm)
! Dc           ::          diffusion coefficient, nm2/s
! vir          ::          virial coefficient
! tscale       ::          temperature scaling factor
! pscale       ::          pressure scaling factor

      MODULE mdcalculations 
      CONTAINS
!     Read into position arrays
      SUBROUTINE readin(pos, v)
      USE PARAMETERS
      IMPLICIT NONE
      INTEGER J, K
      CHARACTER(LEN=5) atomname
      INTEGER atomnum
      CHARACTER(LEN=3) resID
      REAL*8 pos
      DIMENSION pos(np, 3)
      REAL*8 v
      DIMENSION v(np, 3)
      OPEN(UNIT = 11, FILE = inputfile)
      READ(11,*)
      READ(11,*)

!     Import the argon particle file into module here
      DO J = 1, np
        READ(11, '(i5,2a5,i5,3f8.3,3f8.4)')atomnum,atomname,resID, atomnum, (pos(J, k), k= 1, 3), (v(J, k), k = 1, 3) 
      END DO
      CLOSE(UNIT = 11)
      RETURN
      END

      SUBROUTINE compute_forces(pos, forces, PE)
      USE PARAMETERS
      IMPLICIT NONE
      INTEGER J, K
      INTEGER Ja, Jb
      REAL*8 pos
      DIMENSION pos(np, 3)
      REAL*8 forces
      DIMENSION forces(np, 3)
      REAL*8 dist
      DIMENSION dist(3)
      REAL*8 r_cent 
      REAL*8 PE 
      REAL*8 force_cent 
      REAL*8 r6, r12 
      REAL*8 force_comp
      REAL*8 PE_lrc
      REAL*8 sr3
      REAL, PARAMETER :: pi = 4.0 * ATAN(1.0)
      REAL, PARAMETER :: vol = length*length*length
      REAL, PARAMETER :: density = np/vol

      ! Initialize force array to 0.0
         DO J = 1, np
            DO K=1, 3  
               forces(J, k) = 0.0
            END DO
         END DO

!     Calculate forces: pairwise distances between all particles
!     making sure not to repeat any particle here
!     (force of particle i on j is equal to the force of particle j on i)
         DO Ja = 1, np - 1
            DO Jb = Ja+1, np
               DO k = 1, 3
                   dist(k) = pos(Ja, k) - pos(Jb, k)
                   dist(k) = dist(k) - ANINT(dist(k)/length)*length
               END DO          
               r_cent = 0.0
               DO k = 1,3
                  r_cent = r_cent + dist(k)**2
               END DO

               ! only compute forces if closer than cutoff
               IF (r_cent .LT. Rc) THEN 
                  r_cent = SQRT(r_cent)
                  r6 = (sigma / r_cent)**6 
                  r12 = r6**2 
                  PE = PE + (r12 - r6) 
                  force_cent = (24.0*eps*1E9/r_cent*(2.0*r12-r6)) 
                  DO k = 1, 3
                     force_comp = dist(k) * force_cent / r_cent 
                     forces(Ja,k) = forces(Ja,k) + force_comp 
                     forces(Jb,k) = forces(Jb,k) - force_comp 
                  END DO
               END IF

               ! forces beyond cutoff distance is 0
               IF (r_cent.GT.Rc) THEN
               force_cent = 0.0
               END IF
            END DO
         END DO
         PE = 4.0 * eps * PE
         sr3 = 1.0 / Rc**3
         PE_lrc = pi*eps*( (8.0/9.0)*sr3**3  - (8.0/3.0)* sr3 )*density
         PE = PE + PE_lrc 
      RETURN
      END

!     Leapfrog for updating position, velocity and forces of particles
      SUBROUTINE upd_particles(pos, v, forces, KE, sys_temp, prestensor)
      USE PARAMETERS
      IMPLICIT NONE
      INTEGER J, K
      REAL*8 sys_temp
      REAL*8 sys_pres
      REAL*8 pos
      DIMENSION pos(np, 3)
      REAL*8 v
      DIMENSION v(np, 3)
      REAL*8 forces
      DIMENSION forces(np, 3)
      REAL*8 KE
      REAL*8 acc
      REAL*8 old_vel
      REAL*8 tscale
      REAL*8 vir
      REAL*8 r_cent
      REAL*8 Dc
      REAL, PARAMETER :: vol = length*length*length
      REAL*8 force_cent
      REAL, PARAMETER :: density = np/vol
      REAL*8 pi
      REAL*8 sys_pres_lrc
      REAL*8 sr3
      REAL*8 pos0
      DIMENSION pos0(np, 3)
      REAL*8 vel0
      DIMENSION vel0(np, 3)
      REAL*8 vacf
      REAL*8 A
      DIMENSION A(np,3)
      REAL*8 msd
      REAL*8 B
      DIMENSION B(np,3)
      REAL*8 virten
      DIMENSION virten(np,1)
      DIMENSION kinten(np,1)
      REAL*8 kinten
      REAL*8 virtensor
      REAL*8 kintensor
      REAL*8 prestensor
      CHARACTER(LEN=5) atomname
      INTEGER atomnum
      CHARACTER(LEN=3) resID
      OPEN(UNIT = 11, FILE = inputfile)
      READ(11,*)
      READ(11,*)

      ! Leapfrog algorithm
      KE = 0.0
      DO J = 1, np
         DO k = 1, 3
            old_vel = v(J, k)
            acc = forces(J,k) * ( 1E-15 / mass)
            v(J, k) = old_vel +  dt * acc 
            KE = KE + ( 0.5*( v(J,k) + old_vel ) )**2 
            pos(J, k) = pos(J, k) + dt*v(J, k) 
         END DO
      END DO
      KE = 0.5*mass*1E6*KE 

      DO J = 1, np
         virten = pos(J,2)*forces(J,1)
         kinten = v(J,2)*v(J,1)
      END DO
         virtensor = SUM(virten)
         kintensor = SUM(kinten)
         prestensor = virtensor + kintensor

      ! To calculate the diffusion coefficient (from mean-squared displacement)
      Dc = 0.0
      DO J = 1, np
         DO K = 1, 3
            Dc = Dc + r_cent**2/(np*dt)
         END DO
      END DO

      ! To calculate temperature
      sys_temp = KE/(1.5*np*boltz_k)

      ! NVT simulation (thermostat), using the velocity rescaling method
      IF (Tcoupl .EQV. .TRUE.) THEN
         tscale = SQRT(Ref_T/sys_temp)
         DO J = 1, np
            DO K = 1, 3
               v(J, k) = v(J, k) * tscale
            END DO
         END DO
      END IF

      ! To calculate pressure (using the virial)
      DO J = 1, np
         DO K = 1,3
            vir = vir - force_cent*r_cent
            sys_pres = (density*sys_temp) + (vir)/(3*vol)
            sys_pres_lrc = pi * ( (32.0/9.0) * sr3**3  - (16.0/3.0) * sr3 ) * density**2
            sys_pres = sys_pres + sys_pres_lrc
         END DO
      END DO

      ! To calculate the VACF
      DO J = 1, np
         READ(11, '(i5,2a5,i5,3f8.3,3f8.4)')atomnum,atomname,resID, atomnum, (pos0(J, k), k= 1, 3), (vel0(J, k), k = 1, 3) 
       END DO
       CLOSE(UNIT = 11)

      DO J = 1, np
         DO K = 1, 3
            A = v(J,k) * vel0(J,k)
            vacf = SUM(A)/np
         END DO
      END DO

      DO J = 1,np
         DO K = 1,3
            B = (pos(J,k)*10 - pos0(J,k)*10)**2
            msd = SUM(B)/np
         END DO
      END DO


      RETURN
      END
      END MODULE mdcalculations

