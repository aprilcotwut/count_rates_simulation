# NOTE: This script is for editting purposes ONLY, the .xcm script is the only
# one functional with Xspec.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script allows the user to determine the count rates of simulated data  #
# using the tbabs model in Xspec. The parameters which the user wishes to     #
# vary should be set immediately below. The script first develops the         #
# simulated data using "fakeit", then fits the simulated data to a simple     #
# power law model. Count rates for the whole spectral model as well as        #
# individual bands are calculated. The ratio contrasting band count rates     #
# to the total count rate for the model, the count rates for the model        #
# components (comptonization and the disk, as currently set), and the photon  #
# index are all saved in a .txt file. The "spectral.R" script can plot this   #
# data and save it as a csv, or another script can be developed to access     #
# this data.                                                                  #
#                                                                             #
# Changes can be made to use this script with other models (than tbabs with   #
# disk and comptonization), however comments with the tag "NOTE" imply        #
# changes may need to be made in order to make it functional.                 #
#                                                                             #
# Usage: Either type "@spectral" in Xspec from the command line or type       #
# "% xspec - spectral" directly from the command line. Set directory name     #
# when prompted.                                                              #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Please change these parameters as needed! No additional changes need be made
# to the script as a whole other than *potentially* the energy plots - jump to
# comment around line 125 for more info. NOTE: Changing *which* parameters vary
# will require additional changes.
# Fakeit settings (arf and rmf)
set fake1 {rmf.rmf & arf.arf & y & & test.fak & 20.0 1.0 1.0}
# Model components set to vary
set nh_vals {0.01 0.02 0.05 0.10 0.20 0.90 2.70 5.50 11.40}
set norm_vals {100 500 5000 10000 50000}

# The following chunk defines commands already avaliable through Xspec
proc xmodel {modelString param1 args} {
 model $modelString & $param1 & /*
}

proc xfakeit {fakeitSettings param1 args} {
 fakeit $fakeitSettings & $param1 & /*
}

proc xshowall {args} {
  show all /*
}

# # # # # # # # # # #
# Begin main method #
# # # # # # # # # # #

# This sets the name for our new run directory, also files, etc
puts "Please name run directory"
set dirname [gets stdin]
file mkdir $dirname

# Names and open's file to save count rates
set outputCounts [string cat $dirname counts.txt]
set fileC [open $dirname/$outputCounts "w"]

set n [llength $nh_vals]
set m [llength $norm_vals]

array set p {}
array set p_disk {}
array set p_compt {}

set p_powl {0.0 & 1.7 & &}

# Used much later to assess error
set flux_err_lst {}

set i 0
set j 0
set key 1

# NOTE: the following while loop will need to be adjusted if a different model
# is used... This loop makes a matrix of paramater vlues to be fed into the
# xpsec model.
while {$i < $n} {
  while {$j < $m} {

      # nh & Tin(keV) & norm & z & T0(photons) & kT & taup & approx & norm &
      set p1_tmp {0.00 & 1.0 & 100 & 0.0 & 1.0 & 100.0 & 0.01 & & &}
      lset p1_tmp {0} [lindex $nh_vals $i]
      lset p1_tmp {4} [lindex $norm_vals $j]

      set p2_tmp {0.00 & 1.0 & 100 & & & & 0.0 & 1.0 & 100.0 & 0.01 & & &}
      lset p2_tmp {0} [lindex $nh_vals $i]
      lset p2_tmp {4} [lindex $norm_vals $j]

      set p3_tmp {0.00 & & & & 1.0 & 100 & 0.0 & 1.0 & 100.0 & 0.01 & & &}
      lset p3_tmp {0} [lindex $nh_vals $i]
      lset p3_tmp {7} [lindex $norm_vals $j]

      append p($key) $p1_tmp
      append p_disk($key) $p2_tmp
      append p_compt($key) $p3_tmp

    set key [expr {$key + 1}]
    set j [expr {$j + 1}]
  }
  set j 0
  set i [expr {$i + 1}]
}

set n [array size p]

set i 1
query yes
while {$i <= $n} {
  # Make the model. NOTE: changing this will change the model used, change with
  # caution. Another line below will also need to be changed.
  xmodel {tbabs(diskbb+comptt)} $p($i)

  # Fake the data.
  xfakeit {none} $fake1

  # Now lets throw that data back in
  data test.fak
  xmodel {tbabs(po)} $p_powl
  fit

  # Save our gamma (photon index) and print to file
  tclout param 2
  scan $xspec_tclout %f gamma


# This plots the energy, the "ignore" line sets the energy levels to be
# plotted - I have it set to slightly different levels at higher "norm" values
# but NOTE: you may want to adjust this.
  if {$i > [expr $n - 5]} {
    ignore **-1.0 8.0-**
    setplot energy
    # setplot rebin,,10,-1
    cpd ${dirname}/model${i}.ps/cps
    plot ldata
  } else {
    ignore **-0.3 8.0-**
    setplot energy
    # setplot rebin,,10,-1
    cpd ${dirname}/model${i}.ps/cps
    plot ldata
  }

  # This plots the model developed for the fake data, and saves flux
  xmodel {tbabs(diskbb+comptt)} $p($i)
  fit
  plot model

  # Calculate the total flux
  flux
  tclout flux
  scan $xspec_tclout %f flux_tot

  # Calculate flux from diskbb
  xmodel {tbabs(diskbb+cflux*comptt)} $p_disk($i)

  flux
  tclout flux
  scan $xspec_tclout %f flux_comptt

  # Calculate flux from comptt
  xmodel {tbabs(cflux*diskbb+comptt)} $p_compt($i)

  flux
  tclout flux
  scan $xspec_tclout %f flux_diskbb

  set flux_c_tot [expr $flux_diskbb + $flux_comptt]

  # Count rates. Each "ignore" sets the band
  notice all
  ignore **-0.3 10.0-**
  tclout rate all
  scan $xspec_tclout %f rates_all

  ignore **-0.3 1.0-**
  tclout rate all
  scan $xspec_tclout %f rates_1

  notice all
  ignore **-1.0 2.0-**
  tclout rate all
  scan $xspec_tclout %f rates_2

  notice all
  ignore **-2.0 10.0-**
  tclout rate all
  scan $xspec_tclout %f rates_3

  notice all
  ignore **-0.5 7.0-**
  tclout rate all
  scan $xspec_tclout %f rates_4

  notice all
  ignore **-0.5 2.0-**
  tclout rate all
  scan $xspec_tclout %f rates_5

  notice all
  ignore **-2.0 7.0-**
  tclout rate all
  scan $xspec_tclout %f rates_6

  notice all
  ignore **-2.0 4.0-**
  tclout rate all
  scan $xspec_tclout %f rates_7

  notice all
  ignore **-4.0 6.0-**
  tclout rate all
  scan $xspec_tclout %f rates_8

  notice all
  ignore **-6.0 8.0-**
  tclout rate all
  scan $xspec_tclout %f rates_9

  notice all
  ignore **-4.0 8.0-**
  tclout rate all
  scan $xspec_tclout %f rates_10

  # This should be 1, of course
  set ratio_all [expr $rates_all / $rates_all]
  # While all the following are ratios comparing to the rates_all
  set ratio_1 [expr $rates_1 / $rates_all]
  set ratio_2 [expr $rates_2 / $rates_all]
  set ratio_3 [expr $rates_3 / $rates_all]
  set ratio_4 [expr $rates_4 / $rates_all]
  set ratio_5 [expr $rates_5 / $rates_all]
  set ratio_6 [expr $rates_6 / $rates_all]
  set ratio_7 [expr $rates_7 / $rates_all]
  set ratio_8 [expr $rates_8 / $rates_all]
  set ratio_9 [expr $rates_9 / $rates_all]
  set ratio_10 [expr $rates_10 / $rates_all]

  set ratio_flux_1 [expr $flux_diskbb / $flux_c_tot]
  set ratio_flux_2 [expr $flux_comptt / $flux_c_tot]

  set flux_tot_err [expr $flux_tot - $flux_c_tot]
  lappend flux_err_lst $flux_tot_err

  set nh [lindex $p($i) 0]
  set norm [lindex $p($i) 4]

  # Prints all data to a txt file
  puts $fileC [format "%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f" \
      $nh $norm $ratio_all $ratio_1 $ratio_2 $ratio_3 $ratio_4 $ratio_5 \
      $ratio_6 $ratio_7 $ratio_8 $ratio_9 $ratio_10 $ratio_flux_1 \
      $ratio_flux_2 $gamma]

  set i [expr {$i + 1}]
}
close $fileC

# The following commented lines test that the error is approximately zero
#set i 0
#while {$i < $n} {
#  puts [lindex $flux_err_lst $i]
#  set i [expr {$i + 1}]
#}
