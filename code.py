new_data = data.loc[:, ["Unique Key", "Created Date", "Agency", "Complaint Type", "Incident Zip", "Borough", "Latitude", "Longitude"]]
timestamp = new_data["Created Date"].apply(lambda x: x[0:10])
new_data["timestamp"] = timestamp
new_data = new_data.loc[:, ["timestamp", "Unique Key", "Created Date", "Agency", "Complaint Type", "Incident Zip", "Borough", "Latitude", "Longitude"]]
new_data = new_data.dropna()

new_data["stations"] = station_np

########################### weather impute

weather = pd.read_csv("weather_NY_2010_2018Nov.csv")
station_table = weather.loc[:, ["StationName", "Latitude", "Longitude"]].groupby("StationName").mean()
weather["SnowDepth"] = weather["SnowDepth"].fillna(0)

for name in ["MinTemp", "MaxTemp", "DewPoint"]:
    impute_value = weather[name].mean()
    weather[name] = weather[name].fillna(impute_value)

    
for name in ["WindSpeed", "MaxSustainedWind", "Percipitation"]:
    impute_value = weather[name].median()
    weather[name] = weather[name].fillna(impute_value)

    
gust_not_null = weather.loc[:, ["WindSpeed", "Gust", "MaxSustainedWind"]][weather["Gust"].isnull() == False]
gust_null = weather.loc[:, ["WindSpeed", "Gust", "MaxSustainedWind"]][weather["Gust"].isnull() == True]

from sklearn import linear_model
regr = linear_model.LinearRegression()
regr.fit( gust_not_null.loc[:, ["WindSpeed", "MaxSustainedWind"]], gust_not_null.loc[:, "Gust"])

gust_impute = regr.predict(gust_null.loc[:, ["WindSpeed", "MaxSustainedWind"]])
gust_null.loc[:, "Gust"] = gust_impute

weather["Gust"] = pd.concat([gust_null.loc[:, "Gust"], gust_not_null.loc[:, "Gust"]]).sort_index()
weather["timestamp"] = weather["Month"].astype(str).apply(lambda x: x.zfill(2)) + "/" + weather["Day"].astype(str).apply(lambda x: x.zfill(2)) + "/" + weather["Year"].astype(str)

#weather = pd.read_csv("cleaned_weather.csv")
weather = weather.merge(station_table, on="StationName")

ff = new_data.loc[:, ["timestamp", "stations", "Unique Key"]].groupby(["timestamp", "stations"]).count()
ff["timestamp"] = ff.index.get_level_values(0)
ff["stations"] = ff.index.get_level_values(1)
ff.index = range(0, ff.shape[0])
ff.columns = ["n_req", "timestamp", "stations"]
ff.head()


df = ff.merge(weather, how = "inner", left_on = ["timestamp", "stations"], right_on = ["timestamp", "index"])
df.shape #19204,27
df = df.drop(columns=['Unnamed: 0', 'USAF', 'WBAN', 'State', 'Latitude_x', 'Longitude_x', 'Year',
       'Month', 'Day', 'Latitude_y', 'Longitude_y', 'index'])

df["timestamp"] = pd.to_datetime(df["timestamp"])
df = df.sort_values(by = "timestamp")

df_target = df.loc[:, ["n_req", "timestamp"]]
df_target.plot(x = "timestamp", y = "n_req")
df.plot(x = "timestamp", y = "Percipitation")
