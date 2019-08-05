new_data = data.loc[:, ["Unique Key", "Created Date", "Agency", "Complaint Type", "Incident Zip", "Borough", "Latitude", "Longitude"]]
timestamp = new_data["Created Date"].apply(lambda x: x[0:10])
new_data["timestamp"] = timestamp
new_data = new_data.loc[:, ["timestamp", "Unique Key", "Created Date", "Agency", "Complaint Type", "Incident Zip", "Borough", "Latitude", "Longitude"]]
new_data = new_data.dropna()

new_data["stations"] = station_np

########################### weather impute
