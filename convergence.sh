#!/bin/bash
##########################################################################
# convergence.sh is an auxiliary tool for Quantum Espresso Linux users.  # 
# It perform convergence test of kpoints mesh and  energy wave functions #
# cut-off. Copyright (C) <2022>  <J.R.M. Monteiro and Henrique Pecinatto>#
# e-mail: joziano@protonmail.com | pecinatto@ufam.edu.br                 #
# Version 1, Oct 1, 2022                                                 #
##########################################################################
# LICENSE INFORMATION                                                    #
#                                                                        #
# This program is free software: you can redistribute it and/or modify   #
# it under the terms of the GNU General Public License as published by   #
# the Free Software Foundation, either version 3 of the License, or      #
# (at your option) any later version.                                    #
#                                                                        #
# This program is distributed in the hope that it will be useful,        #
# but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
# GNU General Public License for more details.                           #
#                                                                        #
# You should have received a copy of the GNU General Public License      #
# along with this program.  If not, see <https://www.gnu.org/licenses/>5.#
##########################################################################

##########################################################################
# INFORMATION ############################################################
# This program is run in terminal as: bash pdos-sum.sh                   #
##########################################################################

if [ ! -f "$(ls *.scf.in)" ]
then 
  echo "ERROR..."
  echo "scf.in not found..."
  echo "aborting..."
  exit 1
fi

if [ ! -f "control.in" ]
then 
  echo "ERROR..."
  echo "control.in not found..."
  echo "aborting..."
  exit 1
fi

fmt="%-5s%-5s%-5s%-5s\n"
fmt2="%-16s%-16s\n"

grep "\S" control.in > controlTemp.txt
grep "\S" $(ls *.scf.in) > runTemp.txt

SEEDNAME=$(echo $(ls *.scf.in) | sed -e 's/.scf.in//g') 
numberLine=$(wc -l < runTemp.txt)
numberLineControl=$(wc -l < controlTemp.txt)

for i in $(seq 1 1 $numberLine)
do
	echo $(awk 'NR=='$i'{print $0}' runTemp.txt) >> run.txt
done

for i in $(seq 1 1 $numberLineControl)
do
	echo $(awk 'NR=='$i'{print $0}' controlTemp.txt) >> control.txt
done

rm runTemp.txt controlTemp.txt

MYCOMMAND=$(awk 'NR=='2'{print $0}' control.txt) 

KPOINTS=$(echo $(grep '^kpoints' control.txt | perl -pe 's/\s+//g' | sed -e 's/kpoints=//g'))
ECUTWFC=$(echo $(grep '^ecutwfc' control.txt | perl -pe 's/\s+//g' | sed -e 's/ecutwfc=//g'))

KMESHDIM=($(echo $(grep '^kmeshDim' control.txt | perl -pe 's/\s+//g' | sed -e 's/kmeshDim(a,b,c)=//g' | sed -e 's/(//g' | sed -e 's/)//g')))
KMESHBEHAVIOR=($(echo $(grep '^kmeshBehavior' control.txt | perl -pe 's/\s+//g' | sed -e 's/kmeshBehavior=//g' | sed -e 's/(//g' | sed -e 's/)//g')))

LOOPKPOINT=($(echo $(grep '^loopKp' control.txt | perl -pe 's/\s+//g' | sed -e 's/loopKp(start,step,end)=//g' | sed -e 's/(//g' | sed -e 's/)//g')))
LOOPECUTWFC=($(echo $(grep '^loopEcutwfc' control.txt | perl -pe 's/\s+//g' | sed -e 's/loopEcutwfc(start,step,end)=//g' | sed -e 's/(//g' | sed -e 's/)//g')))



if [ "$KPOINTS" == ".TRUE." -a "$ECUTWFC" == ".TRUE." ] || [ "$KPOINTS" == ".true." -a "$ECUTWFC" == ".true." ]
then
  rm control.txt run.txt
  echo "ERROR..."
  echo "kpoints=$KPOINTS and ecutwfc=$ECUTWFC"
  echo "Here, the kpoints and ecutwfc convergence test are mutually exclusive..."
  echo "read the informations in control.in file..."
  echo "aborting..."
  exit 1
fi 

if [ "$KPOINTS" == ".FALSE." -a "$ECUTWFC" == ".FALSE." ] || [ "$KPOINTS" == ".false." -a "$ECUTWFC" == ".false." ]
then
  rm control.txt run.txt
  echo "ERROR..."
  echo "kpoints=$KPOINTS and ecutwfc=$ECUTWFC"
  echo "read the informations in control.in file..."
  echo "aborting..."
  exit 1
fi 
                                                                                                                       
if [ "$KPOINTS" == ".FALSE." -a "$ECUTWFC" == ".TRUE." ] || [ "$KPOINTS" == ".false." -a "$ECUTWFC" == ".true." ]
then
  if [ -d "./energy" ]; then rm -r ./energy; fi
  INITIALECUT=$(echo $LOOPECUTWFC | cut -d, -f1)
  STEPECUT=$(echo $LOOPECUTWFC | cut -d, -f2)
  FINALECUT=$(echo $LOOPECUTWFC | cut -d, -f3)
  mkdir energy
  for ECUT in $(seq $INITIALECUT $STEPECUT $FINALECUT)                        
  do 
    for i in $(seq 1 1 $numberLine)
    do
      writingLine=$(awk 'NR=='$i'{print $0}' run.txt)
      if [ "$writingLine" == "$(grep '^ecutwfc' run.txt)" ]
      then
        echo "ecutwfc = $ECUT" >> ${SEEDNAME}.${ECUT}.in
      else
        echo $(awk 'NR=='$i'{print $0}' run.txt) >> ${SEEDNAME}.${ECUT}.in
      fi
    done

    mv ${SEEDNAME}.${ECUT}.in ./energy

    cd energy
      $MYCOMMAND < ${SEEDNAME}.${ECUT}.in > ${SEEDNAME}.${ECUT}.out  
      if [ $ECUT == $INITIALECUT ]; then printf "$fmt2" ecutwfc"(Ry)" Total-energy"(Ry)" >> ecut.txt;fi
      ETOT=$(echo $(grep ! ${SEEDNAME}.${ECUT}.out ) | sed -e "s/! total energy =//g" | sed -e "s/Ry//g")
      printf "$fmt2" $ECUT $ETOT >> ecut.txt
      rm -f ${SEEDNAME}.${ECUT}.out 
      rm -f ${SEEDNAME}.${ECUT}.in 
    cd .. 
  done
  rm control.txt run.txt
fi

if [ "$KPOINTS" == ".TRUE." -a "$ECUTWFC" == ".FALSE." ] || [ "$KPOINTS" == ".true." -a "$ECUTWFC" == ".false." ]
then
  if [ -d "./kpoints" ]; then rm -r ./kpoints; fi
  sed -i '$d' run.txt
  INITIALKP=$(echo $LOOPKPOINT | cut -d, -f1)
  STEPKP=$(echo $LOOPKPOINT | cut -d, -f2)
  FINALKP=$(echo $LOOPKPOINT | cut -d, -f3)
  mkdir kpoints
  for KP in $(seq $INITIALKP $STEPKP $FINALKP)                        
  do
    if [ "$(echo $KMESHBEHAVIOR | cut -d, -f1)" == "k1[VAR]" ] || [ "$(echo $KMESHBEHAVIOR | cut -d, -f1)" == "k1[var]" ]
    then                                                                                                                           
      Mk1=$(echo $KMESHDIM | cut -d, -f1)
      k1=$(($Mk1*$KP))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f1)" == "k1[FIX]" ]
    then
      k1=$(echo $(grep '^k1\[FIX\]' control.in | sed -e "s/k1\[FIX//g" | sed -e "s/\]=//g"))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f1)" == "k1[fix]" ]
    then
      k1=$(echo $(grep '^k1\[fix\]' control.in | sed -e "s/k1\[fix//g" | sed -e "s/\]=//g"))
    fi

    if [ "$(echo $KMESHBEHAVIOR | cut -d, -f2)" == "k2[VAR]" ] || [ "$(echo $KMESHBEHAVIOR | cut -d, -f2)" == "k2[var]" ]
    then                                                                                                                           
      Mk2=$(echo $KMESHDIM | cut -d, -f2)
      k2=$(($Mk2*$KP))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f2)" == "k2[FIX]" ]
    then
      k2=$(echo $(grep '^k2\[FIX\]' control.in | sed -e "s/k2\[FIX//g" | sed -e "s/\]=//g"))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f2)" == "k2[fix]" ]
    then
      k2=$(echo $(grep '^k2\[fix\]' control.in | sed -e "s/k2\[fix//g" | sed -e "s/\]=//g"))
    fi

    if [ "$(echo $KMESHBEHAVIOR | cut -d, -f3)" == "k3[VAR]" ] || [ "$(echo $KMESHBEHAVIOR | cut -d, -f3)" == "k3[var]" ]
    then                                                                                                                           
      Mk3=$(echo $KMESHDIM | cut -d, -f3)
      k3=$(($Mk3*$KP))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f3)" == "k3[FIX]" ]
    then
      k3=$(echo $(grep '^k3\[FIX\]' control.in | sed -e "s/k3\[FIX//g" | sed -e "s/\]=//g"))
    elif [ "$(echo $KMESHBEHAVIOR | cut -d, -f3)" == "k3[fix]" ]
    then
      k3=$(echo $(grep '^k3\[fix\]' control.in | sed -e "s/k3\[fix//g" | sed -e "s/\]=//g"))
    fi
 
    for i in $(seq 1 1 $numberLine)
    do
      writingLine=$(awk 'NR=='$i'{print $0}' run.txt)

      if [ "$writingLine" == "$(grep '^K_POINTS' run.txt)" ]
      then
          echo $(awk 'NR=='$i'{print $0}' run.txt) >> ${SEEDNAME}.${k1}x${k2}x${k3}.in
          echo "$k1 $k2 $k3 0 0 0" >> ${SEEDNAME}.${k1}x${k2}x${k3}.in
      else
          echo $(awk 'NR=='$i'{print $0}' run.txt) >> ${SEEDNAME}.${k1}x${k2}x${k3}.in
      fi
    done

    mv ${SEEDNAME}.${k1}x${k2}x${k3}.in ./kpoints

    cd kpoints

      $MYCOMMAND < ${SEEDNAME}.${k1}x${k2}x${k3}.in > ${SEEDNAME}.${k1}x${k2}x${k3}.out
      if [ $KP == $INITIALKP ]; then printf "$fmt" k1 k2 k3 Total-energy"(Ry)" >> kp.txt;fi
      ETOT=$(echo $(grep ! ${SEEDNAME}.${k1}x${k2}x${k3}.out) | sed -e "s/! total energy =//g" | sed -e "s/Ry//g")
      printf "$fmt" $k1 $k2 $k3 $ETOT >> kp.txt
      rm -f ${SEEDNAME}.${k1}x${k2}x${k3}.out  
      rm -f ${SEEDNAME}.${k1}x${k2}x${k3}.in 
    cd ..
  done
  rm control.txt run.txt  
fi

exit 0
