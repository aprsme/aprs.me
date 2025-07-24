// Type definitions for leaflet.vectorgrid
import * as L from 'leaflet';

declare module 'leaflet' {
  namespace vectorGrid {
    interface VectorGridOptions extends L.GridLayerOptions {
      rendererFactory?: L.Renderer;
      vectorTileLayerStyles?: any;
      interactive?: boolean;
      getFeatureId?: (feature: any) => string | number;
    }

    interface ProtobufOptions extends VectorGridOptions {
      subdomains?: string | string[];
      key?: string;
      token?: string;
      maxNativeZoom?: number;
    }

    class VectorGrid extends L.GridLayer {
      constructor(options?: VectorGridOptions);
      setFeatureStyle(id: string | number, style: L.PathOptions): void;
      resetFeatureStyle(id: string | number): void;
      clearHighlight(): void;
    }

    class Protobuf extends VectorGrid {
      constructor(url: string, options?: ProtobufOptions);
    }

    function protobuf(url: string, options?: ProtobufOptions): Protobuf;
  }

  function vectorGrid(options?: any): any;
}

export {};