#!/usr/bin/env python3

# make_stats_tables.py
# Using SciPy to genate statistical distribution tables for use in
# PRNG_Zoo

import numpy, scipy, datetime
from scipy import stats


print("""
-- Generated from SciPy version {0} on {1}
-- Statistical distribution tables\n""".format( \
		scipy.version.full_version,\
		datetime.date.today().isoformat()))
print("""

package PRNG_Zoo.Stats.Tables is
""")

# Normal

print("""-- Normal distribution
-- Survival function (1.0 - cdf from -inf to x)
-- for x from 0.00 to 5.00 in 0.01 intervals""")
print("""Normal : constant array(Positive range <>) of Long_Float :=(""")

for i in range(0,501):
	x = i/100.0
	print("{0:.15E}".format(stats.norm.sf(x)), end='')
	if i %4 == 3:
		print(",")
	elif i != 500:
		print(", ", end='')
	else:
		print("\n);\n")


# Chi2
#stats.chi2(df=1).ppf(upper_critical_values)

upper_critical_values = [0.95, 0.99, 0.999, 0.999999]
lower_critical_values = [0.05, 0.01, 0.001, 0.000001]

print("""-- Chi2 distribution""")

critical_values = ", ".join(["{0:.15f}".format(x) for x in lower_critical_values])

print("Chi2_Critical_Values : constant array(Positive range 1..4) of Long_Float :=\n ({0});\n".format(critical_values))

print("""-- Upper critical values for degrees of freedom from 1 to 256""")

print("""Chi2_Upper : constant array(1..256, 1..4) of Long_Float :=(""")

for i in range(1,257):
	print("({0:.15f}, {1:.15f}, {2:.15f}, {3:.15f})".format(*stats.chi2(df=i).ppf(upper_critical_values)), end='')
	if i == 256:
		print("\n);\n")
	else:
		print(",")

print("""Chi2_Lower : constant array(1..256, 1..4) of Long_Float :=(""")

for i in range(1,257):
	print("({0:.15f}, {1:.15f}, {2:.15f}, {3:.15f})".format(*stats.chi2(df=i).ppf(lower_critical_values)), end='')
	if i == 256:
		print("\n);\n")
	else:
		print(",")

print("""end PRNG_Zoo.Stats.Tables;""")
