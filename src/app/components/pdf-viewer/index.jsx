'use client';

import { useCallback, useState } from 'react';
import { useResizeObserver } from '@wojtekmaj/react-hooks';
import { pdfjs, Document, Page } from 'react-pdf'
import 'react-pdf/dist/esm/Page/AnnotationLayer.css';
import 'react-pdf/dist/esm/Page/TextLayer.css';

pdfjs.GlobalWorkerOptions.workerSrc = new URL(
  'pdfjs-dist/build/pdf.worker.min.js',
  import.meta.url,
).toString();

const options = {
  cMapUrl: '/cmaps/',
  standardFontDataUrl: '/standard_fonts/',
};

const resizeObserverOptions = {};

const PDFViewer = ({file, maxWidth, portfolio}) => {
  const [numPages, setNumPages] = useState();
  const [containerRef, setContainerRef] = useState(null);
  const [containerWidth, setContainerWidth] = useState();

  const onResize = useCallback((entries) => {
    const [entry] = entries;

    if (entry) {
      setContainerWidth(entry.contentRect.width);
    }
  }, []);

  useResizeObserver(containerRef, resizeObserverOptions, onResize);

  function onDocumentLoadSuccess({ numPages: nextNumPages }) {
    setNumPages(nextNumPages);
  }

  return (
    <div ref={setContainerRef}>
      <Document className={`${portfolio && '[&_canvas]:mx-auto'}`} file={file} onLoadSuccess={onDocumentLoadSuccess} options={options}>
        {Array.from(new Array(portfolio ? 1 : numPages), (_, index) => (
          <Page
            key={`page_${index + 1}`}
            pageNumber={index + 1}
            width={containerWidth ? Math.min(containerWidth, maxWidth) : maxWidth}
          />
        ))}
      </Document>
    </div>
  );
};

export default PDFViewer;

