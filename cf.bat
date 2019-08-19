set CFPATHFILES = C:\
rem imputation by CF and CBR
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=1 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f1_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f1_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=1 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f1_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f1_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=2 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f2_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f2_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=2 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f2_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f2_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=3 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f3_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f3_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=3 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f3_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f3_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=4 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f4_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f4_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=4 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f4_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f4_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=5 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f5_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f5_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=5 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f5_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f5_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=6 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f6_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f6_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=6 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f6_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f6_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=7 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f7_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f7_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=7 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f7_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f7_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=8 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f8_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f8_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=8 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f8_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f8_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=9 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f9_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f9_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=9 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f9_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f9_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=10 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f10_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f10_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_log_fit.csv" -ns=10 -p=WeightedSum -eo="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f10_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_log_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_log_fit.csv_Projedt Actual Time,_f10_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=1 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f1_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f1_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=1 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f1_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f1_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=2 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f2_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f2_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=2 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f2_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f2_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=3 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f3_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f3_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=3 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f3_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f3_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=4 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f4_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f4_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=4 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f4_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f4_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=5 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f5_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f5_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=5 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f5_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f5_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=6 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f6_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f6_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=6 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f6_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f6_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=7 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f7_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f7_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=7 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f7_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f7_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=8 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f8_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f8_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=8 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f8_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f8_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=9 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f9_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f9_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=9 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f9_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f9_Std_EuclideanDistance _WS.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=10 -p=AmplifiedWeightedSumWithAveragedMultiplier -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f10_Nor_AdjCosWMed_AmpWSWAvgdMul.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=normalize -s=AdjustedCosineSimilarityWithMedian -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f10_Nor_AdjCosWMed_AmpWSWAvgdMul.csv"
docfbe.exe -e="%CFPATHFILES%\data_fit.csv" -ns=10 -p=WeightedSum -eo="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f10_Std_EuclideanDistance _WS.txt" -l="%CFPATHFILES%\data_fit.csv" -t="Projedt Actual Time, Effort Plan, Effort Specify" -n=standardize -s=EuclideanDistance  -o="%CFPATHFILES%\data_fit.csv_Projedt Actual Time,_f10_Std_EuclideanDistance _WS.csv"
