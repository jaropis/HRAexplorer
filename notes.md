## This curl works perfectly with the api

curl -X POST "http://localhost:8000/numerical-results" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "analysis_type=poincare" \
  -d 'fileAddresses=["../initial_data/0011.rea","../initial_data/0043_.rea","../initial_data/0047.rea","../initial_data/0072.rea"]' \
  -d "separator=%09" \
  -d "column_data=\"2 3\"" \
  -d "minmax=\"0 3000\"" \
  -d "using_excel=false" \
  -d "use_ULF=No" \
  -d 'flags_coding={"sinus":"0","ventricular":"1","supraventricular":"2","artefact":"3"}' \
  -d "shuffle=false" \
  -d "pnnX_th=[30,50]" \
  -d "pnn_perc_th=[5,10]" \
  -d "sampen_m=2" \
  -d "sampen_r=0.15" \
  -d "location=results.xlsx"
