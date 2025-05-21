import streamlit as st
from streamlit_folium import st_folium
import folium
import pandas as pd
import plotly.express as px
from its_live.datacube_tools import DATACUBETOOLS as dctools

def descargar_series_tiempo(coords_list, variable="v"):
    dct = dctools()
    dfs = []
    for lat, lon in coords_list:
        # OJO: El widget original usa [lon, lat]
        ins3xr, ds_point, point_tilexy = dct.get_timeseries_at_point([lon, lat], "4326", variables=[variable])
        if ins3xr is not None and point_tilexy is not None:
            # Selecciona la celda más cercana, igual que el widget
            export = ins3xr[
                ["v", "v_error", "vx", "vx_error", "vy", "vy_error", "date_dt", "satellite_img1", "mission_img1"]
            ].sel(x=point_tilexy[0], y=point_tilexy[1], method="nearest")
            df = export.to_dataframe().reset_index()
            df["lat"] = lat
            df["lon"] = lon
            dfs.append(df)
    if dfs:
        return pd.concat(dfs, ignore_index=True)
    else:
        return pd.DataFrame()

st.title("Descarga de series de tiempo ITS_LIVE")
st.write("Haz clic en el mapa para seleccionar un punto. Luego grafica los datos.")

# Estado para el punto seleccionado
if "coords" not in st.session_state:
    st.session_state.coords = None

# Mapa base
m = folium.Map(location=[70, -45], zoom_start=3)

# Agregar marcador si hay punto seleccionado
if st.session_state.coords:
    lat, lon = st.session_state.coords
    folium.Marker(
        location=[lat, lon],
        popup=f"({lat:.4f}, {lon:.4f})",
        icon=folium.Icon(color="red")
    ).add_to(m)

# Capa de velocidad ITS_LIVE
vel_layer = folium.raster_layers.TileLayer(
    tiles="https://glacierflow.nyc3.digitaloceanspaces.com/webmaps/vel_map/{z}/{x}/{y}.png",
    attr='ITS_LIVE velocity mosaic (https://its-live.jpl.nasa.gov/)',
    name="ITS_LIVE Velocity Mosaic",
    overlay=True,
    control=True,
    opacity=0.7
)
vel_layer.add_to(m)

# Otras capas opcionales (ejemplo: coastlines)
coastlines = folium.raster_layers.TileLayer(
    tiles="https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/Coastlines_15m/default/GoogleMapsCompatible_Level13/{z}/{y}/{x}.png",
    attr="NASA GIBS Imagery",
    name="Coastlines",
    overlay=True,
    control=True,
    opacity=0.7
)
coastlines.add_to(m)

# Control de capas
folium.LayerControl().add_to(m)

# Mostrar mapa en Streamlit
map_data = st_folium(m, width=700, height=500)

# Obtener coordenada seleccionada por clic
if map_data and map_data["last_clicked"]:
    lat = map_data["last_clicked"]["lat"]
    lon = map_data["last_clicked"]["lng"]
    st.session_state.coords = (lat, lon)
    st.rerun()

# Permitir agregar manualmente el punto
manual = st.text_input("Agregar coordenada manualmente (lat,lon):")
if manual:
    try:
        lat, lon = map(float, manual.split(","))
        st.session_state.coords = (lat, lon)
        st.rerun()
    except:
        st.warning("Formato incorrecto. Usa lat,lon")

# Botón para limpiar punto
if st.button("Limpiar punto"):
    st.session_state.coords = None
    st.rerun()

# Mostrar punto seleccionado
if st.session_state.coords:
    lat, lon = st.session_state.coords
    st.write(f"Punto seleccionado: ({lat:.4f}, {lon:.4f})")

# Graficar datos
if st.session_state.coords and st.button("Graficar serie de tiempo"):
    with st.spinner("Descargando y graficando datos..."):
        df = descargar_series_tiempo([st.session_state.coords])
    if df.empty:
        st.warning("No se encontraron datos para la coordenada seleccionada.")
    else:
        print(df)
        df["mid_date"] = pd.to_datetime(df["mid_date"])
        df = df.sort_values("mid_date")
        fig = px.line(
            df,
            x="mid_date",
            y="v",
            labels={"v": "Velocidad (m/año)", "mid_date": "Fecha"},
            title="Serie de tiempo de velocidad ITS_LIVE"
        )
        st.plotly_chart(fig, use_container_width=True)
        st.dataframe(df)