# Data Showroom 

Build interactive visualizations to explore data on your own computer or upload them to a server to embed on a website. The tool is tailored, but not exclusive, to the GC Wealth Data Warehouse (see https://github.com/gcwealthproject/warehouse) 

## Interactive examples

A published interactive version, exploring estimates of wealth concentration in the United States from many sources, is published in the following [link](https://ign-flores.shinyapps.io/rshiny_inht_1/). 

<div align="center">
<img src="screenshots/screenshot1.png" width="450" />
<img src="screenshots/screenshot2.png" width="450" />  
</div>

## Menu configuration

The application’s menu is fully customizable and defined in a YAML file. This makes it easy to update or modify the menu structure without changing the application code. Simply write a yaml file or edit the `config.yaml` file located in the `config` directory to make adjustments.

Below is an example snippet from the `menu.yaml` file:


```yaml 

#Define fixed selectors 
fixed_sel:
  GEO_long:
    label: "Country"
    selected:
      - "United States"
  var_per:
    label: "Variable"
    selected: "Share of Total Net Wealth, p99p100"
  d5_dboard_specific_lab:
    label: "Unit of analysis"
    selected:
      - "Individuals - Adults"
      - "Individuals - Adults (equal split)"
      - "Households"
      - "Tax Units"
    multiple: true

#Define reactive selectors (react to changes in fixed selectors)
reactive_sel:
  Data_Type:
    label: "Data type"
    type: "checkbox"
  legend:
    label: "Source"
    type: "checkbox"

```

For a fuller configuration, see the [test configuration](tests/config.yaml) file.



