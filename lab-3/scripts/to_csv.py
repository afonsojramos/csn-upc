import pandas as pd
f = open("./results/ER.txt","r")
previous_language = "Arabic"
c_df = pd.DataFrame()
column = []
for line in f.readlines():
    try:
        value = float(line.replace("\n",""))
        column.append(value)
    except ValueError:
        language = line.replace("\n","")
        if language != "Arabic":
            c_df[previous_language] = column
            previous_language = language
            column = []
c_df[previous_language] = column
c_df.to_csv("./results/ER.csv")