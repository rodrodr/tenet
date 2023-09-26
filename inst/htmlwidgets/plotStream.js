HTMLWidgets.widget({

  name: 'plotStream',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

          var root = am5.Root.new(el.id);


          root.setThemes([
              am5themes_Animated.new(root)
          ]);
  
  
          // Create chart
          // https://www.amcharts.com/docs/v5/charts/xy-chart/
          var chart = root.container.children.push(am5xy.XYChart.new(root,         {
            panX: false,
            panY: false,
            wheelX: "panX",
            wheelY: "zoomX",
            layout: root.horizontalLayout
          }));
          
          
          // Add legend
          // https://www.amcharts.com/docs/v5/charts/xy-chart/legend-xy-series/
          var legend = chart.children.push(
              am5.Legend.new(root, {
                position: "top",
                layout: root.horizontalLayout,
                clickTarget: "itemContainer"
              })
            );

          
          legend.valueLabels.template.set("forceHidden", true);

        
        var data = x.data;
  
        var xAxis = chart.xAxes.push(am5xy.CategoryAxis.new(root, {
          categoryField: "category",
          startLocation: 0.5,
          endLocation: 0.5,
          renderer: am5xy.AxisRendererX.new(root, {
            minGridDistance: 40
          }),
          tooltip: am5.Tooltip.new(root, {})
        }));
        
        xAxis.data.setAll(data);
        
        var yAxis = chart.yAxes.push(am5xy.ValueAxis.new(root, {
          renderer: am5xy.AxisRendererY.new(root, {})
        }));
        
        
        // Add series
        // https://www.amcharts.com/docs/v5/charts/xy-chart/series/
        function createSeries(field, name) {
          var series = chart.series.push(am5xy.SmoothedXLineSeries.new(root, {
            name: name,
            xAxis: xAxis,
            yAxis: yAxis,
            valueField: field,
            valueYField: field + "_hi",
            openValueYField: field + "_low",
            categoryXField: "category",
            tooltip: am5.Tooltip.new(root, {
              pointerOrientation: "horizontal",
              labelText: "[fontSize: 18px]{name}[/]\n{categoryX}: [bold]{" + field + "}[/]"
            })
          }));
          
          // Do not show tooltip for zero values
          series.get("tooltip").adapters.add("visible", function(visible, target) {
            if (target.dataItem && (target.dataItem.get("value") > 0)) {
              return true;
            }
            return false;
          });
        
          series.strokes.template.setAll({
            forceHidden: true
          });
          
          series.fills.template.setAll({
            visible: true,
            fillOpacity: 1
          });

        chart.get("colors").set("colors", [x.colors]);

         // Make stuff animate on load
        // https://www.amcharts.com/docs/v5/concepts/animations/
        series.appear();
      
        legend.data.push(series);
      }
        
        
      var ser = x.series;
      
      for (var k = 0; k < ser.length; k++) {
          createSeries(ser[k].var, ser[k].desc);
      }


      // Prepare data for the river-stacked series
      for (var i = 0; i < data.length; i++) {
        var row = data[i];
        var sum = 0;
      
        // Calculate open and close values
        chart.series.each(function(series) {
          var field = series.get("valueField");
          var val = Number(row[field]);
          row[field + "_low"] = sum;
          row[field + "_hi"] = sum + val;
          sum += val;
        });
      
        // Adjust values so they are centered
        var offset = sum / 2;
        chart.series.each(function(series) {
          var field = series.get("valueField");
          row[field + "_low"] -= offset;
          row[field + "_hi"] -= offset;
        });
        
        chart.series.each(function(series) {
          var field = series.get("valueField");
          series.data.setAll(data);
        });
      
      }
      
      
      // Add cursor
      chart.set("cursor", am5xy.XYCursor.new(root, {
        behavior: "zoomXY",
        xAxis: xAxis
      }));
      
      
      // Make stuff animate on load
      // https://www.amcharts.com/docs/v5/concepts/animations/
      chart.appear(1000, 100);
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
