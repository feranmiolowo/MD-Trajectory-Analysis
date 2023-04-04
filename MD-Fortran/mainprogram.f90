! md_mainprogram.f - main program for MD simulation

      PROGRAM mdsimulation
      USE mdcalculations
      USE parameters
      IMPLICIT NONE
      INTEGER I
      REAL*8 pos(np, 3), v(np, 3), forces(np, 3)
      REAL*8 PE, KE, sys_temp, sys_pres, vacf, msd, prestensor
      
      CALL readin(pos, v)

! Main MD loop
      DO I = 1, nsteps
         CALL compute_forces(pos, forces, PE)
         CALL upd_particles(pos, v, forces, KE, sys_temp, prestensor)
         CALL o_f(I, pos, v, PE, KE, sys_temp, prestensor)
      END DO
      END

      SUBROUTINE o_f(frame, pos, v, PE, KE, sys_temp, prestensor)
      USE parameters
      IMPLICIT NONE
      INTEGER I, K
      INTEGER frame
      REAL*8 pos(np, 3), v(np, 3)
      REAL*8 PE, KE, sys_temp, sys_pres, Dc, vir, forces(np,3)
      REAL*8 time, TE, vacf, msd, virtensor, kintensor, prestensor

      IF (frame .EQ. 1) THEN
         OPEN(UNIT=91, FILE='trajectory.xyz')
         OPEN(UNIT=92, FILE='energy.log')
         OPEN(UNIT=93, FILE='temperature.log')
         OPEN(UNIT=95, FILE='pressure.log')
         OPEN(UNIT=96, FILE='virialcoeff.log')
         OPEN(UNIT=97, FILE='diffusioncoeff.log')
         OPEN(UNIT=98, FILE='forces.log')
      END IF

      IF (mod((frame-1),stride) .EQ. 0) THEN
         DO I = 1, np
            ! convert positions to Angstroms for xyz file
            WRITE(91,*) (pos(I,k)*10,k=1,3)
         END DO
         time = dt * (frame - 1)
         TE = PE + KE
         WRITE(92,*) time, PE, KE, TE
         WRITE(93,*) time, sys_temp
         WRITE(95,*) time, length, sys_pres
         WRITE(96,*) time, vir
         WRITE(97,*) time, prestensor
         WRITE(98,*) time, forces(np,3)
         
         ! print occasional updates on simulation status
         IF (mod(frame-1,stride*10) .EQ. 0) THEN
            PRINT*, "Current time step: ", frame
         END IF
      END IF

      IF (frame .eq. nsteps) THEN
         OPEN(UNIT=94, FILE='end_structure_production.gro')
         WRITE(94,*) 'MD simulation of Argon'
         WRITE(94,*) np
         DO I = 1, np
         WRITE(94, 10) I, 'ARGON', 'Ar', I, (pos(I,k),k=1,3),(v(I,k), k=1,3)
         END DO


         CLOSE(91)
         CLOSE(92)
         CLOSE(93)
         CLOSE(94)
      END IF
  10  FORMAT(i5,2a5,i5,3f8.3,3f8.4)
      END 
