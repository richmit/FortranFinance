!----------------------------------------------------------------------------------------------------------------------------------
program utest_stats_probit
  use mrffl_config, only: rk=>mrfflrk
  use mrffl_stats,  only: probit

  implicit none (type, external)

  print '(a30,es20.8,a20)',   "probit(-1.0_rk)",          probit(-1.0_rk),           "Neg Inf"
  print '(a30,es20.8,a20)',   "probit( 2.0_rk)",          probit( 2.0_rk),           "Pos Inf"

  print '(a30,f20.8,f20.8)',  "probit(0.5_rk)",           probit(0.5_rk),            0.0000000000000000_rk
  print '(a30,f20.8,f20.8)',  "probit(0.076_rk)",         probit(0.076_rk),         -1.4325026941760215_rk
  print '(a30,f20.8,f20.8)',  "probit(0.924_rk)",         probit(0.924_rk),          1.4325026941760222_rk

  print '(a30,f20.8,f20.8)',  "probit(0.070_rk)",         probit(0.070_rk),         -1.4757909861707035_rk
  print '(a30,f20.8,f20.8)',  "probit(0.950_rk)",         probit(0.950_rk),          1.6448535815446468_rk

  print '(a30,f20.8,f20.8)',  "probit(1.0e-11_rk)",       probit(1.0e-11_rk),       -6.7060231366218757_rk
  print '(a30,f20.8,f20.8)',  "probit(0.99999999999_rk)", probit(0.99999999999_rk),  6.7060231245414865_rk

  print '(a30,f20.8,f20.8)',  "probit(0.1000_rk)",        probit(0.1000_rk),        -1.2815515152306318_rk
  print '(a30,f20.8,f20.8)',  "probit(0.0100_rk)",        probit(0.0100_rk),        -2.3263478213385005_rk
  print '(a30,f20.8,f20.8)',  "probit(0.0010_rk)",        probit(0.0010_rk),        -3.0902322446474262_rk
  print '(a30,f20.8,f20.8)',  "probit(0.0001_rk)",        probit(0.0001_rk),        -3.7190164113083730_rk

  print '(a30,f20.8,f20.8)',  "probit(0.9000_rk)",        probit(0.9000_rk),         1.2815515152306318_rk
  print '(a30,f20.8,f20.8)',  "probit(0.9900_rk)",        probit(0.9900_rk),         2.3263478213385005_rk
  print '(a30,f20.8,f20.8)',  "probit(0.9990_rk)",        probit(0.9990_rk),         3.0902322446474262_rk
  print '(a30,f20.8,f20.8)',  "probit(0.9999_rk)",        probit(0.9999_rk),         3.7190164113084019_rk

  print '(a30,f20.8,f20.8)',  "probit(0.025_rk)",         probit(0.025_rk),         -1.9599639350393654_rk

end program utest_stats_probit
