####################################################################################################################################################################
# Joset A. Etzel, jetzel@wustl.edu. 
# This file contains pairings for MZ, DZ, non-twin sibling, and unrelated people. Twin status is genotype-confirmed. MZ.1s[1] is the twin
# of MZ.2s[1], etc. (matched by vector index). Each person is listed by "Jo ID", rather than HCP ID; a separate key is needed for matching the IDs.
# the GTunique part of the name reflects that all of these twins have genotype-confirmed twin status, and all groups are unique (no person
# appears more than once).
####################################################################################################################################################################

# usage: get.ids("MZ.1s"), etc. possible values for which.group are at the bottom of the function.

get.ids <- function(which.group) {
  # MZ twins: 105 pairs
  MZ.1s.GTunique <- c("up8860", "ei1052", "xg2390", "xa9091", "sb5631", "yj4297", "hs7790", "xf8217", "zb7543", "ny4028", "if340", "ui8428", "mt2271", 
                      "fe2287", "ou5515", "qa3051", "yj734", "im7779", "lg2253", "gj911", "lo8118", "tj3064", "ld7855", "xq2137", "br8986", "gm7412", 
                      "mw63", "do8174", "mr6988", "pr2765", "cj9450", "qg6812", "ai7564", "hd2079", "nr6957", "kg6220", "za7204", "oc1318", "ty7521", 
                      "hk7512", "fr3903", "du6146", "sb9036", "xj1636", "hf1295", "oh9470", "nn8253", "te8356", "bq457", "lk7580", "rr7211", "xp8514", 
                      "ex4669", "ga3285", "oa5981", "ud5547", "ew2469", "hi7640", "fh2699", "it1787", "cx9808", "rm7208", "ec4184", "zi2331", "vw905", 
                      "st7251", "nn4900", "pv8315", "wx5916", "wc5601", "kj5306", "uc1939", "zl8476", "oj9096", "lp2952", "cm1911", "zy4127", "hv4891", 
                      "ob9396", "lj5511", "kc6064", "co1303", "tx8504", "oj3626", "og1699", "yx5998", "sg5218", "ye5967", "fb9559", "xg5438", "oc4214", 
                      "hx8470", "df8145", "is8342", "mg4903", "jh4223", "pk6257", "vr394", "kr5213", "uq2088", "zb4289", "mo8661", "wo4163", "ml2532", "yw5740");
  MZ.2s.GTunique <- c("th4838", "ss2996", "ct4171", "zn9000", "ql1697", "mn3104", "es3029", "gr828", "zk7167", "ki2142", "gw5507", "ks348", "rk9191", 
                      "qf4888", "zn4946", "ic7015", "ux1568", "ze7706", "hs4364", "dd933", "de7266", "id8755", "fh2020", "zx3526", "xp7855", "so162", 
                      "jp188", "sk2752", "yp5099", "da6972", "vk3353", "ch6597", "cw9972", "ll535", "im9891", "ng7537", "ep71", "gg1348", "ns4482", 
                      "xe4685", "js2597", "jz6566", "bc2101", "vd961", "yp1477", "aa4754", "er3262", "qs7667", "tv7987", "vx4380", "ny3419", "fe7088", 
                      "uz6747", "dn1529", "lj976", "sq1489", "ky2646", "wd8292", "hd7266", "ke3467", "hp3693", "uo3527", "pe1963", "oo3267", "vb3700", 
                      "pk2743", "qr1108", "hx7747", "vm6983", "tg8735", "tm5450", "dr9204", "vv8305", "sd2522", "xn1650", "nt1800", "yq7598", "vw7278", 
                      "az8784", "gu9420", "rh1122", "my7044", "bf9837", "go1166", "pg7963", "el164", "vw7617", "sx7276", "dh4351", "nh5795", "pm3264", 
                      "cq601", "ep2070", "tr2659", "vk6725", "ie8225", "ag1213", "lg5745", "bz9463", "pj8577", "wr5202", "cg8943", "qp2811", "ma7928", "pl6619");
  
  # DZ twins: 78 pairs
  DZ.1s.GTunique <- c("bb4934", "wr8519", "qi9723", "uh4238", "nj9775", "kc3518", "nw2252", "lf5535", "oa9751", "gt185", "pb1903", "pb8936", "ng3506", 
                      "ix4227", "vn7650", "su3645", "nx8660", "rm8505", "qd3657", "fd2525", "ca5018", "jd1964", "wi5744", "fr2578", "ns7330", "ig328", 
                      "xv7888", "dy4972", "vl2882", "tz3071", "et1430", "ct6231", "lu4060", "tx5670", "qz8329", "lh5049", "ry5739", "ub22", "sv1050", 
                      "zp8076", "sh9073", "ho4452", "qz8248", "cl2509", "cs9691", "uw6754", "sx7909", "jh1505", "oy6431", "tj8522", "ss6388", "bx1564", 
                      "th6641", "ti5445", "dc2170", "qh8067", "ix9468", "fu4952", "lq147", "bu7334", "pe8461", "jb8336", "bl1733", "ng7390", "fs9681", 
                      "ob7283", "xl995", "nu868", "gc2234", "jb2591", "dw4092", "wq7313", "jf6228", "db6412", "ag5213", "ie8874", "ov2980", "vr4751");
  DZ.2s.GTunique <- c("zy9688", "fp916", "bz274", "rc7697", "hl7390", "hq3709", "gm1079", "kt8002", "jc6773", "cs5197", "dt1372", "tb4452", "kk7321", 
                      "kq2702", "mp3370", "la196", "rm5865", "qs9711", "tq2115", "sm1241", "ba2743", "hw3664", "ry6595", "dq5352", "li2052", "fp6017", 
                      "ie7709", "vm7506", "cx4373", "ss3114", "pb924", "sr182", "ck6395", "he6807", "aa4340", "gr5225", "xx3869", "ia4019", "ob5169", 
                      "fu4651", "tr3019", "au2746", "nr1944", "iu339", "hb7546", "vb2722", "bn7978", "ri764", "qk7176", "qq5491", "ne7334", "we7146", 
                      "to1698", "xj6341", "lw521", "ok7328", "qw3315", "ty3311", "tp4003", "gm3042", "nb5136", "sl1458", "va3536", "cn2854", "of1252", 
                      "be7256", "ny6429", "jz5835", "jh9757", "qr1593", "gb9984", "nl1099", "vu5503", "ux6584", "oz1883", "bm1248", "pc4750", "aj8654");
  
  # SIB (non-twin siblings; gender-matched and +- 3 years of age): 99 pairs
  sib.1s.GTunique <- c("it8300", "xs553", "gg702", "nl5754", "iw879", "cc6132", "yr434", "xa9005", "yq180", "ft9400", "na1498", "qk4233", "ve3542", "ap7783", 
                       "gp7471", "rb4829", "wp3000", "pq8384", "td8421", "dj7661", "fs930", "ww8145", "st45", "le5590", "jf3344", "wk2336", "ft9011", "xl7979", 
                       "tr9549", "jp8139", "vs5826", "ah673", "vc5430", "pb8774", "dc4172", "yz5396", "gk1347", "gs4172", "qu1531", "eo7543", "dn1689", "ie5118", 
                       "ja6509", "jn8238", "bk9727", "dh2607", "ht9924", "xd5612", "kk3557", "wz822", "nw3907", "ga7671", "dz7934", "qx1048", "ft8109", "nd6153", 
                       "th3887", "zy9293", "yu3541", "gn6655", "wq5000", "un7328", "cj2737", "rq4571", "pt2977", "rx7568", "ir2672", "ad4777", "rc9209", "vu5230", 
                       "of468", "oy2140", "zc316", "di3023", "qk6014", "da1986", "zx4695", "bl7466", "xt1347", "qv7391", "qg1670", "jw1043", "ih4890", "ve4948", 
                       "kb5117", "mf8367", "dd6703", "rb6609", "zl5642", "ko6022", "rg1774", "ce5091", "ej1029", "qc512", "be2767", "op8531", "ev642", "ca495", 
                       "nt4314"); 
  sib.2s.GTunique <- c("pc7540", "zn7696", "qe5259", "dx2691", "cg2804", "ad7210", "el5413", "pv9887", "pp4389", "fg3071", "nu7275", "ou9400", "tw3690", 
                       "do8148", "yq7999", "fh8461", "xj7604", "xz6210", "zw7120", "rr456", "ah4097", "vo6203", "de8281", "vt9052", "nn6131", "ch9899", 
                       "gf8020", "es640", "hq3334", "zj6300", "kt9829", "mn306", "ju1958", "kg1432", "gj790", "af3627", "mp1889", "lb2474", "hu327", "zw9397", 
                       "ly5693", "dt6892", "en2879", "xj3439", "it1088", "jr7919", "bb440", "zy9792", "cx3929", "ku6307", "bf116", "es6625", "zl3796", "oq7881", 
                       "jd9162", "cx5169", "pe8166", "tq4370", "uk1141", "vp1634", "oy7328", "sc9142", "ym8681", "vm8923", "yr4229", "ot7589", "iq5754", "sf5720", 
                       "ah8396", "gn9389", "zv5196", "kr4867", "qe5420", "xe2884", "rw7136", "mj2086", "uw5997", "bd8517", "aw7272", "ks3309", "hq2337", "mw2274", 
                       "hf6589", "lu1607", "jk1749", "az5848", "mp4941", "hk1453", "ky6043", "uk8158", "ty1974", "sr7236", "so7034", "hx1258", "pa7039", "lf4691", 
                       "rj9981", "cz9241", "mo8565");
  
  # UNR (unrelated people, but gender-matched and +- 3 years of age): 100 pairs
  unr.1s.GTunique <- c("xi7403", "th3865", "cr148", "kk2358", "vp5697", "fz7166", "vj9648", "mn3829", "ex8507", "ba1212", "xl4234", "pa1649", "zq5666",
                       "wv5494", "jb5735", "di3276", "yc3953", "sm8332","cx4463", "gk193", "ok9336", "hi4", "wr637", "mv6385", "gk4777", "hn492", "ho7321", 
                       "nm9643", "sq7540", "em4298", "rw3835", "hw3146", "kz945", "pi718", "xl7300", "xf5953","ds6990", "qh8768", "ii3675", "ft2978", "vk699", 
                       "kt7781", "pf1072", "ez9041", "ct513", "fk4173", "tx698", "vd2554", "gg6680", "pv6031", "ju9775", "uk6414", "vt4915", "pc3748", "vl7071", 
                       "re6196", "go4030", "oq5597", "qs4081", "vx530", "xq5560", "vh3017", "qp5652", "vh6049", "ri2117", "wa3344", "wl8439", "hw2805", "da1191", 
                       "nu337", "yj7782", "xv7254", "my1072", "id188", "oh6604", "ad9315", "xy515", "dn9510", "yz8678", "ji526", "qw6902", "dy8299", "ch6641", 
                       "wx8150", "it4689", "rm6723", "xf129", "fn3311", "gg2881", "hm6425", "ha6918", "ha6858", "bx2695", "uj6835", "wl5297", "ox4018", "nw5058", 
                       "hp4157", "jc6534", "wl1711");
  unr.2s.GTunique <- c("vj3313", "gm2212", "gv5523", "rl6413", "jv3246", "wz5737", "yr3375", "lh8474", "yr465", "ul9467", "na2143", "yf889", "wd8632", "pl5006", 
                       "lg2964", "zf2566", "jj546", "qp7874", "fe5970", "to310", "an3359", "qt7134", "be48", "nb2856", "oo2117", "og5198", "qi8592", "gc7272", 
                       "yp6652", "dt2936", "ez9963", "va7234", "ay5994", "xp8815", "vm3849", "bg7946", "qr9070", "rq6067", "bd1678", "cv2036", "mw4145", "to4928", 
                       "ou3514", "kn8119", "xr442", "gz4507", "vs9439", "dr9939", "db9667", "nf701", "pf4871", "ze9693", "lv2906", "fd517" , "pf298", "vk9147", 
                       "vj9920", "cw2550", "ep1790", "tx3850", "mj1492", "hr2878", "dx4634", "to4873", "ei5289", "by6072", "dm6523", "gr3041", "aa2207", "sc1919", 
                       "qp5149", "dd1868","ox2573", "ca1129", "sn9878", "zk5849", "ml9653", "op9792", "hf3833", "ia9878", "rb7843", "za4766", "yq1164", "rl9673", 
                       "sp7960", "av7881", "oo2005", "zs5172", "tr7220", "od2605","ex476", "gy5943", "xb3564", "sd9694", "zw5400", "gd4335", "ff8679", "kv9422", 
                       "eg7558", "lv8272");
 
  ids <- NA;
  if (which.group == "MZ.1s.GTunique" | which.group == "MZ.1s") { ids <- MZ.1s.GTunique; }
  if (which.group == "MZ.2s.GTunique" | which.group == "MZ.2s") { ids <- MZ.2s.GTunique; }
  if (which.group == "DZ.1s.GTunique" | which.group == "DZ.1s") { ids <- DZ.1s.GTunique; }
  if (which.group == "DZ.2s.GTunique" | which.group == "DZ.2s") { ids <- DZ.2s.GTunique; }
  if (which.group == "sib.1s.GTunique" | which.group == "SIB.1s") { ids <- sib.1s.GTunique; }
  if (which.group == "sib.2s.GTunique" | which.group == "SIB.2s") { ids <- sib.2s.GTunique; }
  if (which.group == "unr.1s.GTunique" | which.group == "UNR.1s") { ids <- unr.1s.GTunique; }
  if (which.group == "unr.2s.GTunique" | which.group == "UNR.2s") { ids <- unr.2s.GTunique; }
  
  return(ids);
}

####################################################################################################################################################################

