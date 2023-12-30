'use client';

import { useRef } from 'react';
import PDFViewer from '../components/pdf-viewer';

export default function CV() {
  const resumeRef = useRef('./resume.pdf');
  const maxWidth = 1200;

  return (
    <>
      <h1 className="text-5xl font-medium mb-10">Resume</h1>
      <PDFViewer file={resumeRef.current} maxWidth={maxWidth} />
    </>
  )
}
