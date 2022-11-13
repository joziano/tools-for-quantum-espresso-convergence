# Please, when using this tool, cite:

J. R. M. Monteiro, & Henrique Pecinatto. (2022). convergence.sh/tools-for-quantum-espresso (v1.0). Zenodo. https://doi.org/10.5281/zenodo.7316945


# convergence.sh 

**1.** **convergence.sh is an auxiliary tool for Linux Quantum Espresso users**. 
        
      Perform convergence test of kpoints mesh and  energy wave functions cut-off.
   
**2.** **The control.in file**

      Control the parameters of convergence test.

**3.** **Required files** 
       
      control.in, convergence.sh and work file scf.in should be in the same directory.
   
**4.** **After control.in configuration, the convergence.sh should be run in the Linux terminal as:** 
      
      bash convergence.sh

**5.** **Control.in parameters informations: logical and string variables**
                                                     
      - Is accepted .TRUE. or .true. and .FALSE. or .false.                 
      - Is accepted VAR or var and FIX or fix                               
      - k-point syntax: k1, k2, k3 (lowercase)                              

**6.** **Control.in environment**
   
   **6.1** **&quantumEspressoCommand**
   
      Quantum espresso Command Path configuration
  
   **6.2** **&convergenceTestOption**
                                                   
     Possible configuration: kpoints=.TRUE. and ecutwfc=.FALSE. or kpoints=.FALSE. and ecutwfc=.TRUE.
     The kpoints and ecutwfc convergence test are mutually exclusive. 
                                                                      
   **6.3** **&kpoints** 
   
     Configuration required only if kpoints=.TRUE. and ecutwfc=.FALSE., otherwise it is ignored.                           
                                                                      
   **6.4** **&ecutwfc** 
   
     Configuration required only if if ecutwfc=.TRUE. and kpoints=.FALSE., otherwise it is ignored.                           
                                                                      
   **6.5** **kmeshDim(a, b, c)=(Mk1, Mk2, Mk3)**  
   
     Mk1, Mk2 and Mk3 are related to crystal dimensions in reciprocal space. If, for example, the crystal is cubic, the a, b and c        
     dimensions are equal and we can set kmeshDim(a,b,c)=(1,1,1). If, for example, the crystal have equal a and b dimensions, but a   
     and b are twice the size of c, we can set kmeshDim=(2,2,1).         
                                                                      
   **6.6** **kmeshBehavior=(k1[OPTION], k2[OPTION], k3[OPTION])** 
   
     Possible configuration: OPTION=VAR or OPTION=FIX

     If OPTION=VAR, the correspondent kpoint (k1, k2 or k3) is given according to 6.7.   

     If OPTION=FIX, it is necessary to specify the fixed value. For example, if throughout the kpoint convergence test we want     
     to keep k3 fixed and equal to 2, we must set:
     
     kmeshBehavior=(k1[VAR], k2[VAR], k3[FIX])                             
     k3[FIX]=2 

     If throughout the kpoint convergence test we want to keep k1 and k2 fixed and equal to 1, we must set:                           
                                                                  
     kmeshBehavior=(k1[FIX],k2[FIX],k3[VAR])                             
     k1[FIX]=1                                                           
     k2[FIX]=1                                                           

   **6.7** **loopKp(start, step, end)=(STARTKP, STEPKP, ENDKP)** 
   
     loopKp(start,step,end) variable control the loop of kpoints mesh. The k1, k2 and k3 kpoints, from scf calculation are given by the   
     relation: k1=Mk1xKP, k2=Mk2xKP, and k3=Mk3xKP, for KP from sequence STARTKP,STEPKP,ENDKP. If, for example, is set              
     kmeshDim(a,b,c)=(2,2,1), kmeshBehavior=(k1[VAR], k2[VAR], k3[VAR]) and loopKp(start,step,end)=(2,1,6), kpoint mesh will be run in 
     scf calculation as: 4x4x2, 6x6x3, 8x8x4, 10x10x5 12x12x6. If, for example, is set kmeshDim(a,b,c)=(1,1,1), kmeshBehavior=(k1[VAR], k2[VAR], k3[VAR]) 
     and loopKp(start,step,end)=(2,2,8), kpoints will be run in scf calculation as: 2x2x2, 4x4x4, 6x6x6, and 8x8x8.                     
                                                                      
   **6.8** **loopEcutwfc(start, step, end)=(STARTECUT, STEPECUT, ENDECUT)**          

     loopEcutwfc(start,step,end) variable control the loop of ecutwfc. If, for example, is set loopEcutwfc(start,step,end)=(40,10,80)      
     ecutwfc will be run in scf calculation as: 40, 50, 60, 70, 80 Ry. If, for example, is set loopEcutwfc(start,step,end)=(60,20,120)     
     ecutwfc will be run in scf calculation as: 60, 80, 100, 120 Ry.     
