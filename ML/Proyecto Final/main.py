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
        ins3xr, ds_point, point_tilexy = dct.get_timeseries_at_point([lon, lat], "4326", variables=[variable])
        if ins3xr is not None and point_tilexy is not None:
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

st.title("ITS_LIVE! Predicción de series de tiempo de Velocidad de Glaciares")
st.write("Haz clic en el mapa para seleccionar un punto. O usa los campos manuales para mover el marcador.")

# Estado para el punto seleccionado
if "coords" not in st.session_state:
    st.session_state.coords = None
if "manual_lat" not in st.session_state:
    st.session_state.manual_lat = 70.0
if "manual_lon" not in st.session_state:
    st.session_state.manual_lon = -45.0

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

# Si se selecciona un punto en el mapa, actualiza los inputs y el marcador
if map_data and map_data["last_clicked"]:
    lat = map_data["last_clicked"]["lat"]
    lon = map_data["last_clicked"]["lng"]
    # Actualiza los valores ANTES de crear los widgets
    st.session_state.coords = (lat, lon)
    st.session_state.manual_lat = lat
    st.session_state.manual_lon = lon
    st.rerun()

def actualizar_coords():
    st.session_state.coords = (st.session_state.manual_lat, st.session_state.manual_lon)
    st.rerun()

# Inputs numéricos para latitud y longitud sincronizados y reactivos
col1, col2 = st.columns(2)
with col1:
    st.number_input(
        "Latitud",
        min_value=-90.0,
        max_value=90.0,
        format="%.6f",
        key="manual_lat",
        on_change=actualizar_coords
    )
with col2:
    st.number_input(
        "Longitud",
        min_value=-180.0,
        max_value=180.0,
        format="%.6f",
        key="manual_lon",
        on_change=actualizar_coords
    )

# Selector de intervalo de días
st.write("Selecciona el intervalo de separación de días entre imágenes satelitales:")
min_dt, max_dt = st.slider(
    "Intervalo (días)",
    min_value=0,
    max_value=365,
    value=(5, 90),
    step=1
)

# Graficar datos
if st.session_state.coords and st.button("Graficar serie de tiempo"):
    with st.spinner("Descargando y graficando datos..."):
        df = descargar_series_tiempo([st.session_state.coords])
        df = df.dropna(subset=["v"]) 
    if df.empty:
        st.warning("No se encontraron datos para la coordenada seleccionada.")
    else:
        df["mid_date"] = pd.to_datetime(df["mid_date"])

        # Convertir date_dt a días (si es timedelta)
        if pd.api.types.is_timedelta64_dtype(df["date_dt"]):
            df["dias"] = df["date_dt"].dt.total_seconds() / 86400
        else:
            df["dias"] = df["date_dt"]

        # Filtrar por intervalo de días
        df_filtrado = df[(df["dias"] >= min_dt) & (df["dias"] <= max_dt)]
        df_filtrado = df_filtrado.sort_values("mid_date")
        if df_filtrado.empty:
            st.warning("No hay datos en el intervalo seleccionado.")
        else:
            # Gráfico de puntos
            fig = px.scatter(
                df_filtrado,
                x="mid_date",
                y="v",
                labels={"v": "Velocidad (m/año)", "mid_date": "Fecha"},
                title=f"Serie de tiempo de velocidad ITS_LIVE ({min_dt}-{max_dt} días)",
                trendline="lowess"
            )

            st.plotly_chart(fig, use_container_width=True)
            st.dataframe(df_filtrado)