# Overview ---------------------------------------------------------------------

# PW: EnquirySTAR1#

# From the first email with data

# I have attached one hospital sites data above for the year 2018 (will text
# password as requested).  The data is separated into patient level (‘all data’),
# day and night, and also into daily level, again split into day and night.
# The other three hospital sites data will be organized in a similar manner.
#
# What we are focusing on is how staffing levels affect Patient Experience
# Times(PET), Time to Triage(TTT), and wait to be seen.  We have included both
# actual staffing levels and rostered, taking the actual as the number of staff
# present during that shift.
#
# Included within the dayshift data is number of patients on trolleys for that
# shift (TrolleyGAR). This is also of interest to us and its fluctuations given
# different staffing levels.
#
# We were considering a logistic regression model, but would be very interested
# in any and all suggestions you may have.
#
# Another element of our project is forecasting number of patients that will use
# ED services, so as to roster an appropriate number of staff on any given day.
# To formulate this projection of future staffing requirements, is there any
# additional data you may need? We can look into obtaining 2017’s admissions and
# any other pertinent information.



# From the email with the hospital site data
# I have attached the data sets from each of the
# four pilot sites : Hospital 4,5,6,7.
#
# Each file pertains to one hospital site and includes patient experience times,
# time to triage, wait time, patient characteristics (triage category etc.) and
# staff levels (rostered and actual) for day shifts and nights shifts.
#
# In areas where # is present or where there is a blank this represents missing
# data that we are unable to attain from the hospitals.
#
# Just a few points of clarification:
#
#   •	Hospital 7 is a Local Injury Unit and hence the data available for this
# site is minimal.
#
# •	In relation to Hospital 4- approximately 7,921 patients are classified as
# “eye treatment”- these patients are not treated in the ED but instead receive
# treatment within a separate eye clinic area however, this data from both sites
# is collected in tandem.
#
# •	In relation to Hospital 6 (Matter): some patients are classified as being treated in
# “ED Clinic Other”- this refers to a medic run clinic that is within the ED but
# not staffed by ED nurses. It usually tends to relate to patients who are
# being reviewed or followed up with. We are presently working on compiling the
# staff numbers for this site but will forward within the coming days.
#
# •	The TrolleyGAR variable relates to the number of patients that were recorded
# as being on trolleys on that particular day.
#
# Abbreviations used:
# LOS=Length of Stay
# PET=Patient Experience Time
# TTT=Time to Triage
# ANP=Advanced Nurse Practitioner


# Notes from Jan 8, 2020

# Do I have "Leave without being seen" data?                               QUERY

# Is trolly a good proxy for capacity                                      QUERY

# As trollies fill, staff who would nomrally be working on triage move "to the
# back" to deal with trollies, so times go up.

# PET time relationship to staffing                                         GOAL

# Need clarity on 75 y 9 hours (doesn't match PET)                         QUERY

# Get code book definitions from Vera                                      QUERY


# Times:

# Arrival - could be some lag from the hospital
# Triage
# Wait seens - time from triage to doctor or senior nurse
# Discharge

# PET = Arrival to discharge

# Ways of assessing need:
# Nursing hours per patient visist "required"
# Triage score X nursing hours
# Noleen algorithm
# Total numbers of patients per day
# Monday and Tuesday are the busiest

# Patients are in for multiple shifts
# Staffing only for the first shift a person is there for
# Admitted vs non-admitted

# Daily data: patients per day, nurse hours per patient visit, trolly gar
# Do I have these?                                                         QUERY


# Later update (Gearoid)

# Now I only have data from the Mater Predictors - 24 hour by shift data
# Required RN per shift AN:AP Day and Night shift but patients on day shift
# still on as night shift so be careful.

# Columns AR:AZ Required/rostered RN by 24 hours

# Main outcomes: - leaving without being seen - time to triage - ed care time
# (first triage to discharge or admission)

# Other outcomes - Triage to seen - ED to seen

# Jan 1 missing data

# AQ:D2 Row 136 times off for these; 137 on


# Questions to Gearoid Dec 21, 2020
# I do have one substantial (to me) query about the dates/times. We have ed
# arrival, triage, seen, and ed departure. I would assume that the dates/times
# for these events should always be in that order. However, there are about 2k
# of them that aren’t. It’s not that many (our of >100k observations) but I also
# wanted to confirm with you whether these are likely to be in error or not.
#
# Also, can you please confirm the meaning of the column “New Attendances”? Just
# so I can match up in my head with what I am looking at with respect to
# repeated visits by the same patient. I’ll probably work away on this through
# Wednesday and then disappear for real – but know that this is the project at
# the top of the list when I’m back in Jan.
#
# Reply
# You're right, they do generally fall in that order but those out of order
# entries are likely to be from how the data comes from the hospital. The raw
# data only provided direct date and time figures for ed arrival and ed dept.
# For triage and seen times, they were given in formats such as the time from
# arrival to triage or the time from seen to dept. I used these figures to
# calculate the triage and seen times from the given arrival and dept. However,
# sometimes these were given in minutes and sometimes they were given in hours
# to one decimal place so I imagine that is where the discrepancy is coming
# from, with the margin of error from the rounded hourly figure. Would the
# majority of cases of those 2k only be off by a few minutes in that case?

# For 'New attendances', first, any patients on visit 1 who had their discharge
# destination marked as returns (numbers 44, 45, or 46), if they had a new
# arrival within 42 days of the ed dept date of visit 1, visit 2 was marked as a
# scheduled return. If patients had a visit 2 within 28 days of a visit 1, and
# the discharge destination of visit 1 was not marked a return, visit 2 was
# marked as 'other return'. Overall returns were then any visit which occurred
# within 28 days of that patients last visit. New attendances were then any
# attendance that wasn't a 'scheduled return' or an 'other return'.


# Sysinfo ----------------------------------------------------------------------

# Scripts/Objectives -----------------------------------------------------------

# Data -------------------------------------------------------------------------

